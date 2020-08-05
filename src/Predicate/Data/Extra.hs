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
     extra promoted functions
-}
module Predicate.Data.Extra (

    Pure2
  , type (<$)
  , type (<*)
  , type (*>)
  , FMapFst
  , FMapSnd
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

  , Coerce
  , Coerce2

  , ProxyT
  , ProxyT'

  , Prime
  , PrimeNext
  , Luhn

  , Assoc
  , Unassoc
  , Catch
  , Catch'
  , Dot
  , RDot
  , K
 ) where
import Predicate.Core
import Predicate.Util
import Predicate.Data.List (Uncons, Unsnoc)
import Predicate.Data.Maybe (JustDef, JustFail)
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Proxy
import Control.Applicative
import Control.Monad (join)
import Data.Kind (Type)
import Control.Comonad
import Data.These
import Data.Coerce
import Control.Lens hiding (iall)

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
-- >>> pz @(Fst Id <$ Snd Id) ("abc",Just 20)
-- PresentT (Just "abc")
--
-- >>> pl @(Fst Id <$ Snd Id) (4,These "xxx" 'a')
-- Present These "xxx" 4 ((<$) 4)
-- PresentT (These "xxx" 4)
--
-- >>> pl @(Fst Id <$ Snd Id) (4,This 'a')
-- Present This 'a' ((<$) 4)
-- PresentT (This 'a')
--
-- >>> pl @(Fst Id <$ Snd Id) (4,Just 'a')
-- Present Just 4 ((<$) 4)
-- PresentT (Just 4)
--
-- >>> pl @(Fst Id <$ Snd Id) (4,Nothing @Int)
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
-- >>> pl @(Snd Id <$ Fst Id) (Just 10,'x')
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
-- >>> pl @(Fst Id <* Snd Id) (Just 4,Just 'a')
-- Present Just 4 ((<*) Just 4 | p=Just 4 | q=Just 'a')
-- PresentT (Just 4)
--
-- >>> pz @(Fst Id <* Snd Id) (Just "abc",Just 20)
-- PresentT (Just "abc")
--
data p <* q
infixl 4 <*

type ArrowRT p q = q <* p

-- | similar to Applicative 'Control.Applicative.*>'
--
-- >>> pl @(Fst Id *> Snd Id) (Just 4,Just 'a')
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
-- >>> pz @(Fst Id <|> Snd Id) (Nothing,Just 20)
-- PresentT (Just 20)
--
-- >>> pz @(Fst Id <|> Snd Id) (Just 10,Just 20)
-- PresentT (Just 10)
--
-- >>> pz @(Fst Id <|> Snd Id) (Nothing,Nothing)
-- PresentT Nothing
--
-- >>> pl @(Fst Id <|> Snd Id) (Just "cdef",Just "ab")
-- Present Just "cdef" ((<|>) Just "cdef" | p=Just "cdef" | q=Just "ab")
-- PresentT (Just "cdef")
--
-- >>> pl @(Fst Id <|> Snd Id) ("cdef","ab"::String)
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
-- pz @(Fst Id $$ Snd Id) ((*16),4)
-- PresentT 64
--
-- pz @(Id $$ "def") ("abc"<>)
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
-- pz @(Snd Id $& Fst Id) ((*16),4)
-- PresentT 64
--
-- pz @("def" $& Id) ("abc"<>)
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
-- >>> pl @(Traverse (If (Gt 3) (Pure Maybe Id) (EmptyT Maybe Id)) Id) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Nothing,Nothing,Nothing,Just 4,Just 5]})
-- PresentT Nothing
--
-- >>> pl @(Traverse (MaybeBool (Le 3) Id) Id) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- PresentT Nothing
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (EmptyT Maybe Id)) Id) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (If (Gt 0) (Pure Maybe Id) (MkNothing _)) Id) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id >= 0) Id) Id) [1..5]
-- Present Just [1,2,3,4,5] ((>>) Just [1,2,3,4,5] | {Sequence Just [1,2,3,4,5] | [Just 1,Just 2,Just 3,Just 4,Just 5]})
-- PresentT (Just [1,2,3,4,5])
--
-- >>> pl @(Traverse (MaybeBool (Id <= 3) Id) Id) [1..5]
-- Present Nothing ((>>) Nothing | {Sequence Nothing | [Just 1,Just 2,Just 3,Nothing,Nothing]})
-- PresentT Nothing
--
data Traverse p q
type TraverseT p q = Map p q >> Sequence

instance P (TraverseT p q) x => P (Traverse p q) x where
  type PP (Traverse p q) x = PP (TraverseT p q) x
  eval _ = eval (Proxy @(TraverseT p q))

-- | similar to fmap fst
--
-- >>> pz @FMapFst (Just (13,"Asf"))
-- PresentT (Just 13)
--
-- >>> pl @FMapFst (Just (1,'x'))
-- Present Just 1 (FMapFst)
-- PresentT (Just 1)
--
-- >>> pl @FMapFst [(1,'x'), (2,'y'), (3,'z')]
-- Present [1,2,3] (FMapFst)
-- PresentT [1,2,3]
--

-- to make this work we grab the fst or snd out of the Maybe so it is a head or not/ is a tail or not etc!
-- we still have access to the whole original list so we dont lose anything!
data FMapFst

instance Functor f => P FMapFst (f (a,x)) where
  type PP FMapFst (f (a,x)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (fst <$> mb)) "FMapFst" []

-- | similar to fmap snd
--
-- >>> pz @FMapSnd (Just ("asf",13))
-- PresentT (Just 13)
--
-- >>> pl @FMapSnd (Just (1,'x'))
-- Present Just 'x' (FMapSnd)
-- PresentT (Just 'x')
--
-- >>> pl @FMapSnd (Nothing @(Char,Int))
-- Present Nothing (FMapSnd)
-- PresentT Nothing
--
-- >>> pl @FMapSnd (Right (1,'x'))
-- Present Right 'x' (FMapSnd)
-- PresentT (Right 'x')
--
-- >>> pl @FMapSnd (Left @_ @(Int,Double) "x")
-- Present Left "x" (FMapSnd)
-- PresentT (Left "x")
--

data FMapSnd

instance Functor f => P FMapSnd (f (x,a)) where
  type PP FMapSnd (f (x,a)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (snd <$> mb)) "FMapSnd" []

-- | just run the effect but skip the value
-- for example for use with Stdout so it doesnt interfere with the \'a\' on the rhs unless there is an failure
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

data p |> q
type SkipLT p q = Skip p >> q
infixr 1 |>

instance P (SkipLT p q) x => P (p |> q) x where
  type PP (p |> q) x = PP (SkipLT p q) x
  eval _ = eval (Proxy @(SkipLT p q))

data p >| q
type SkipRT p q = p >> Skip q
infixr 1 >|

instance P (SkipRT p q) x => P (p >| q) x where
  type PP (p >| q) x = PP (SkipRT p q) x
  eval _ = eval (Proxy @(SkipRT p q))

data p >|> q
type SkipBothT p q = Skip p >> Skip q
infixr 1 >|>

instance P (SkipBothT p q) x => P (p >|> q) x where
  type PP (p >|> q) x = PP (SkipBothT p q) x
  eval _ = eval (Proxy @(SkipBothT p q))

-- | takes the head or default of a list-like object
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
-- >>> :set -XFlexibleContexts
-- >>> pz @(HeadDef (MEmptyT _) Id) ([] :: [SG.Sum Int])
-- PresentT (Sum {getSum = 0})
--
-- >>> pz @(HeadDef (MEmptyT String) '["abc","def","asdfadf"]) ()
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) (Snd Id)) (123,["abc","def","asdfadf"])
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) (Snd Id)) (123,[])
-- PresentT ()
--
-- >>> pl @(HeadDef 9 (Fst Id)) ([],True)
-- Present 9 (JustDef Nothing)
-- PresentT 9
--
-- >>> pl @(HeadDef 9 (Fst Id)) ([1..5],True)
-- Present 1 (JustDef Just)
-- PresentT 1
--
-- >>> pl @(HeadDef 3 (Fst Id)) ([10..15],True)
-- Present 10 (JustDef Just)
-- PresentT 10
--
-- >>> pl @(HeadDef 12 (Fst Id) >> Le 6) ([],True)
-- False ((>>) False | {12 <= 6})
-- FalseT
--
-- >>> pl @(HeadDef 1 (Fst Id) >> Le 6) ([],True)
-- True ((>>) True | {1 <= 6})
-- TrueT
--
-- >>> pl @(HeadDef 10 (Fst Id) >> Le 6) ([],True)
-- False ((>>) False | {10 <= 6})
-- FalseT
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([] :: [Int]))
-- Present [] (JustDef Nothing)
-- PresentT []
--
-- >>> pl @(HeadDef (MEmptyT _) Id) (map (:[]) ([10..14] :: [Int]))
-- Present [10] (JustDef Just)
-- PresentT [10]
--
-- >>> pl @(HeadDef (Fst Id) (Snd Id)) (99,[10..14])
-- Present 10 (JustDef Just)
-- PresentT 10
--
-- >>> pl @(HeadDef (Fst Id) (Snd Id)) (99,[] :: [Int])
-- Present 99 (JustDef Nothing)
-- PresentT 99
--
-- >>> pl @(HeadDef 43 (Snd Id)) (99,[] :: [Int])
-- Present 43 (JustDef Nothing)
-- PresentT 43
--
data HeadDef p q
type HeadDefT p q = JustDef p (q >> Uncons >> FMapFst)

instance P (HeadDefT p q) x => P (HeadDef p q) x where
  type PP (HeadDef p q) x = PP (HeadDefT p q) x
  eval _ = eval (Proxy @(HeadDefT p q))


-- | takes the head of a list or fail
--
-- see 'ConsT' for other supported types eg 'Seq.Seq'
--
-- >>> pz @(HeadFail "dude" Id) ["abc","def","asdfadf"]
-- PresentT "abc"
--
-- >>> pz @(HeadFail "empty list" Id) []
-- FailT "empty list"
--
-- >>> pl @(HeadFail "zz" (Fst Id) >> Le 6) ([],True)
-- Error zz ((>>) lhs failed)
-- FailT "zz"
--
-- >>> pl @((HeadFail "failed1" (Fst Id) >> Le 6) || 'False) ([],True)
-- Error failed1 (||)
-- FailT "failed1"
--
-- >>> pl @((Fst Id >> HeadFail "failed2" Id >> Le (6 -% 1)) || 'False) ([-9],True)
-- True (True || False)
-- TrueT
--
-- >>> pl @(HeadFail "Asdf" Id) ([] :: [()]) -- breaks otherwise
-- Error Asdf (JustFail Nothing)
-- FailT "Asdf"
--
-- >>> pl @(HeadFail (PrintF "msg=%s def" (Fst Id)) (Snd Id)) ("Abc" :: String,[]::[Int])
-- Error msg=Abc def (JustFail Nothing)
-- FailT "msg=Abc def"
--

data HeadFail msg q
type HeadFailT msg q = JustFail msg (q >> Uncons >> FMapFst)

instance P (HeadFailT msg q) x => P (HeadFail msg q) x where
  type PP (HeadFail msg q) x = PP (HeadFailT msg q) x
  eval _ = eval (Proxy @(HeadFailT msg q))

-- | TailDef
--
-- >>> pl @(TailDef '[9,7] (Fst Id)) ([],True)
-- Present [9,7] (JustDef Nothing)
-- PresentT [9,7]
--
-- >>> pl @(TailDef '[9,7] (Fst Id)) ([1..5],True)
-- Present [2,3,4,5] (JustDef Just)
-- PresentT [2,3,4,5]
--
-- >>> pl @(TailDef '[3] (Fst Id)) ([10..15],True)
-- Present [11,12,13,14,15] (JustDef Just)
-- PresentT [11,12,13,14,15]
--

data TailDef p q
type TailDefT p q = JustDef p (q >> Uncons >> FMapSnd)

instance P (TailDefT p q) x => P (TailDef p q) x where
  type PP (TailDef p q) x = PP (TailDefT p q) x
  eval _ = eval (Proxy @(TailDefT p q))


-- | TailFail
--
-- >>> pl @(TailFail (PrintT "a=%d b=%s" (Snd Id)) (Fst Id)) ([]::[()],(4::Int,"someval" :: String))
-- Error a=4 b=someval (JustFail Nothing)
-- FailT "a=4 b=someval"
--

data TailFail msg q
type TailFailT msg q = JustFail msg (q >> Uncons >> FMapSnd)

instance P (TailFailT msg q) x => P (TailFail msg q) x where
  type PP (TailFail msg q) x = PP (TailFailT msg q) x
  eval _ = eval (Proxy @(TailFailT msg q))

-- | LastDef
--
-- >>> pl @(LastDef 9 (Fst Id)) ([],True)
-- Present 9 (JustDef Nothing)
-- PresentT 9
--
-- >>> pl @(LastDef 9 (Fst Id)) ([1..5],True)
-- Present 5 (JustDef Just)
-- PresentT 5
--
-- >>> pl @(LastDef 3 (Fst Id)) ([10..15],True)
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
type LastDefT p q = JustDef p (q >> Unsnoc >> FMapSnd)

instance P (LastDefT p q) x => P (LastDef p q) x where
  type PP (LastDef p q) x = PP (LastDefT p q) x
  eval _ = eval (Proxy @(LastDefT p q))

data LastFail msg q
type LastFailT msg q = JustFail msg (q >> Unsnoc >> FMapSnd)

instance P (LastFailT msg q) x => P (LastFail msg q) x where
  type PP (LastFail msg q) x = PP (LastFailT msg q) x
  eval _ = eval (Proxy @(LastFailT msg q))

-- | InitDef
--
-- >>> pl @(InitDef '[9,7] (Fst Id)) ([],True)
-- Present [9,7] (JustDef Nothing)
-- PresentT [9,7]
--
-- >>> pl @(InitDef '[9,7] (Fst Id)) ([1..5],True)
-- Present [1,2,3,4] (JustDef Just)
-- PresentT [1,2,3,4]
--
-- >>> pl @(InitDef '[3] (Fst Id)) ([10..15],True)
-- Present [10,11,12,13,14] (JustDef Just)
-- PresentT [10,11,12,13,14]
--
data InitDef p q
type InitDefT p q = JustDef p (q >> Unsnoc >> FMapFst)

instance P (InitDefT p q) x => P (InitDef p q) x where
  type PP (InitDef p q) x = PP (InitDefT p q) x
  eval _ = eval (Proxy @(InitDefT p q))

data InitFail msg q
type InitFailT msg q = JustFail msg (q >> Unsnoc >> FMapFst)

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
-- >>> pz @(Prime Id) 2
-- TrueT
--
-- >>> pz @(Map '(Id,Prime Id) Id) [0..12]
-- PresentT [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
--
data Prime p

instance (PP p x ~ a
        , P p x
        , Show a
        , Integral a
        ) => P (Prime p) x where
  type PP (Prime p) x = Bool
  eval _ opts x = do
    let msg0 = "Prime"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = p > 1 && isPrime (fromIntegral p)
        in mkNodeB opts b (msg0 <> showVerbose opts " | " p) [hh pp]

-- | get the next prime number
--
-- >>> pz @(PrimeNext Id) 6
-- PresentT 7
--
-- >>> pz @(ScanN 4 (PrimeNext Id) Id) 3
-- PresentT [3,5,7,11,13]
--
data PrimeNext p

instance (PP p x ~ a
        , P p x
        , Show a
        , Integral a
        ) => P (PrimeNext p) x where
  type PP (PrimeNext p) x = Int
  eval _ opts x = do
    let msg0 = "PrimeNext"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let ret = head $ dropWhile (not . isPrime) [max 0 (fromIntegral p + 1) ..]
        in mkNode opts (PresentT ret) (msg0 <> showVerbose opts " | " p) [hh pp]

-- | Luhn predicate check on last digit
--
-- >>> pz @(Luhn Id) [1,2,3,0]
-- TrueT
--
-- >>> pz @(Luhn Id) [1,2,3,4]
-- FalseT
--
-- >>> pz @(GuardSimple (Luhn Id)) [15,4,3,1,99]
-- FailT "(Luhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])"
--
-- >>> pl @(Luhn Id) [15,4,3,1,99]
-- False (Luhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])
-- FalseT
--
data Luhn p

instance (PP p x ~ [Int]
        , P p x
        ) => P (Luhn p) x where
  type PP (Luhn p) x = Bool
  eval _ opts x = do
    let msg0 = "Luhn"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let xs = zipWith (*) (reverse p) (cycle [1,2])
            ys = map (\w -> if w>=10 then w-9 else w) xs
            z = sum ys
            ret = z `mod` 10
            hhs = [hh pp]
        in if ret == 0 then mkNodeB opts True (msg0 <> " | " <> showL opts p) hhs
           else mkNodeB opts False (msg0 <> " map=" <> showL opts ys <> " sum=" <> showL opts z <> " ret=" <> showL opts ret <> showVerbose opts " | " p) hhs


-- | assoc using 'AssocC'
--
-- >>> pz @Assoc (This (These 123 'x'))
-- PresentT (These 123 (This 'x'))
--
-- >>> pz @Assoc ((99,'a'),True)
-- PresentT (99,('a',True))
--
-- >>> pz @Assoc ((99,'a'),True)
-- PresentT (99,('a',True))
--
-- >>> pz @Assoc (Right "Abc" :: Either (Either () ()) String)
-- PresentT (Right (Right "Abc"))
--
-- >>> pz @Assoc (Left (Left 'x'))
-- PresentT (Left 'x')
--
-- >>> pl @Assoc ((10,'c'),True)
-- Present (10,('c',True)) (Assoc (10,('c',True)) | ((10,'c'),True))
-- PresentT (10,('c',True))
--
-- >>> pl @(Assoc >> Unassoc) ((10,'c'),True)
-- Present ((10,'c'),True) ((>>) ((10,'c'),True) | {Unassoc ((10,'c'),True) | (10,('c',True))})
-- PresentT ((10,'c'),True)
--
data Assoc

class AssocC p where
  assoc :: p (p a b) c -> p a (p b c)
  unassoc :: p a (p b c) -> p (p a b) c
instance AssocC Either where
  assoc (Left (Left a)) = Left a
  assoc (Left (Right b)) = Right (Left b)
  assoc (Right b) = Right (Right b)
  unassoc (Left a) = Left (Left a)
  unassoc (Right (Left b)) = Left (Right b)
  unassoc (Right (Right b)) = Right b
instance AssocC These where
  assoc (This (This a)) = This a
  assoc (This (That b)) = That (This b)
  assoc (That b) = That (That b)
  assoc (These (This a) c) = These a (That c)
  assoc (These (That b) c) = That (These b c)
  assoc (These (These a b) c) = These a (These b c)
  assoc (This (These a b)) = These a (This b)
  unassoc (This a) = This (This a)
  unassoc (That (This b)) = This (That b)
  unassoc (That (That b)) = That b
  unassoc (These a (That c)) = These (This a) c
  unassoc (That (These b c)) = These (That b) c
  unassoc (These a (These b c)) = These (These a b) c
  unassoc (These a (This b)) = This (These a b)

instance AssocC (,) where
  assoc ((a,b),c) = (a,(b,c))
  unassoc (a,(b,c)) = ((a,b),c)

instance (Show (p (p a b) c)
        , Show (p a (p b c))
        , AssocC p
        ) => P Assoc (p (p a b) c) where
  type PP Assoc (p (p a b) c) = p a (p b c)
  eval _ opts pabc =
    let msg0 = "Assoc"
        d = assoc pabc
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d pabc) []

-- | unassoc using 'AssocC'
--
-- >>> pz @Unassoc (These 123 (This 'x'))
-- PresentT (This (These 123 'x'))
--
-- >>> pz @Unassoc (99,('a',True))
-- PresentT ((99,'a'),True)
--
-- >>> pz @Unassoc (This 10 :: These Int (These Bool ()))
-- PresentT (This (This 10))
--
-- >>> pz @Unassoc (Right (Right 123))
-- PresentT (Right 123)
--
-- >>> pz @Unassoc (Left 'x' :: Either Char (Either Bool Double))
-- PresentT (Left (Left 'x'))
--
-- >>> pl @Unassoc (10,('c',True))
-- Present ((10,'c'),True) (Unassoc ((10,'c'),True) | (10,('c',True)))
-- PresentT ((10,'c'),True)
--
data Unassoc

instance (Show (p (p a b) c)
        , Show (p a (p b c))
        , AssocC p
        ) => P Unassoc (p a (p b c)) where
  type PP Unassoc (p a (p b c)) = p (p a b) c
  eval _ opts pabc =
    let msg0 = "Unassoc"
        d = unassoc pabc
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d pabc) []


-- | similar to 'coerce'
--
-- >>> pz @(Coerce (SG.Sum Integer)) (Identity (-13))
-- PresentT (Sum {getSum = -13})
--
-- >>> pl @(Coerce SG.Any) True
-- Present Any {getAny = True} (Coerce Any {getAny = True} | True)
-- PresentT (Any {getAny = True})
--
-- >>> pl @(Coerce Bool) (SG.Any True)
-- Present True (Coerce True | Any {getAny = True})
-- PresentT True
--
data Coerce (t :: k)

instance (Show a
        , Show t
        , Coercible t a
        ) => P (Coerce t) a where
  type PP (Coerce t) a = t
  eval _ opts a =
    let msg0 = "Coerce"
        d = a ^. coerced
    in pure $ mkNode opts (PresentT d) (show01 opts msg0 d a) []

-- can coerce over a functor: but need to provide type of 'a' and 't' explicitly

-- | see 'Coerce': coerce over a functor
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

-- | catch a failure
--
-- >>> pz @(Catch (Succ Id) (Fst Id >> Second (ShowP Id) >> PrintT "%s %s" Id >> 'LT)) GT
-- PresentT LT
--
-- >>> pz @(Len > 1 && Catch (Id !! 3 == 66) 'False) [1,2]
-- FalseT
--
-- >>> pl @(Catch (Resplit "\\d+(" Id) (Snd Id >> MEmptyP)) "123"
-- Present [] (Catch caught exception[Regex failed to compile])
-- PresentT []
--
-- >>> pl @(Catch (OneP Id) 99) [10,11]
-- Present 99 (Catch caught exception[OneP 2 elements])
-- PresentT 99
--
-- >>> pl @(Catch (OneP Id) 99) [10]
-- Present 10 (Catch did not fire)
-- PresentT 10
--
-- >>> pl @(Catch (OneP Id) 'True) [False]  -- cant know that this is FalseT cos is driven by type of the list not the 'True part
-- Present False (Catch did not fire)
-- PresentT False
--
-- >>> pl @(Catch (OneP Id) 'False) [True,True,False]
-- False (Catch caught exception[OneP 3 elements])
-- FalseT
--
-- >>> pl @(Catch (OneP Id) 'True) []
-- True (Catch caught exception[OneP empty])
-- TrueT
--
data Catch p q -- catch p and if fails runs q only on failt

-- | Catch'
--
-- >>> pz @(Catch' (Succ Id) (Second (ShowP Id) >> PrintT "%s %s" Id)) GT
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT"
--
-- >>> pz @(Catch' (Succ Id) (Second (ShowP Id) >> PrintT "%s %s" Id)) LT
-- PresentT EQ
--
-- >>> pl @(Catch' (Failt Int "someval") (PrintT "msg=%s caught(%03d)" Id)) (44 :: Int)
-- Error msg=someval caught(044) (Catch default condition failed)
-- FailT "msg=someval caught(044)"
--
-- >>> pl @(Catch' (OneP Id) (Second (ShowP Id) >> PrintT "msg=%s caught(%s)" Id)) [10,12,13]
-- Error msg=OneP 3 elements caught([10,12,13]) (Catch default condition failed)
-- FailT "msg=OneP 3 elements caught([10,12,13])"
--
-- >>> pl @(Catch' (OneP Id) (PrintT "msg=%s caught(%s)" (Second (ShowP Id)))) [10]
-- Present 10 (Catch did not fire)
-- PresentT 10
--
-- >>> pl @(Catch' (OneP Id) (PrintT "msg=%s err s=%s" (Second (ShowP Id)))) [10,11]
-- Error msg=OneP 2 elements err s=[10,11] (Catch default condition failed)
-- FailT "msg=OneP 2 elements err s=[10,11]"
--
data Catch' p s
type CatchT' p s = Catch p (FailCatchT s) -- eg set eg s=PrintF "%d" Id or PrintF "%s" (ShowP Id)
type FailCatchT s = Fail (Snd Id >> Unproxy) (Fst Id >> s)

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
         let emsg = e ^?! tBool . _FailT -- extract the failt string a push back into the fail case
         qq <- eval (Proxy @q) opts ((emsg, x), Proxy @(PP p x))
         pure $ case getValueLR opts (msg0 <> " default condition failed") qq [hh pp] of
            Left e1 -> e1
            Right _ -> mkNode opts (_tBool qq) (msg0 <> " caught exception[" <> emsg <> "]") [hh pp, hh qq]
      Right _ -> pure $ mkNode opts (_tBool pp) (msg0 <> " did not fire") [hh pp]



-- | compose simple functions
--
-- >>> pl @(Dot '[Thd,Snd,Fst] Id) ((1,(2,9,10)),(3,4))
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

-- | reversed dot
--
-- >>> pl @(RDot '[Fst,Snd,Thd] Id) ((1,(2,9,10)),(3,4))
-- Present 10 (Thd 10 | (2,9,10))
-- PresentT 10
--
-- >>> pl @(RDot '[Fst,Snd] Id) (('a',2),(True,"zy"))
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

-- | creates a constant expression ignoring the second argument
--
-- >>> pl @(RDot '[Fst,Snd,Thd,K "xxx"] Id) ((1,(2,9,10)),(3,4))
-- Present "xxx" (K '"xxx")
-- PresentT "xxx"
--
-- >>> pl @(RDot '[Fst,Snd,Thd,K '("abc",Id)] Id) ((1,(2,9,10)),(3,4))
-- Present ("abc",((1,(2,9,10)),(3,4))) (K '("abc",((1,(2,9,10)),(3,4))))
-- PresentT ("abc",((1,(2,9,10)),(3,4)))
--
-- >>> pl @(Thd $ Snd $ Fst $ K Id "dud") ((1,("W",9,'a')),(3,4))
-- Present 'a' (Thd 'a' | ("W",9,'a'))
-- PresentT 'a'
--
-- >>> pl @((Thd $ Snd $ Fst $ K Id "dud") >> Pred Id) ((1,("W",9,'a')),(3,4))
-- Present '`' ((>>) '`' | {Pred '`' | 'a'})
-- PresentT '`'
--
data K (p :: k) (q :: k1)
instance P p a => P (K p q) a where
  type PP (K p q) a = PP p a
  eval _ = eval (Proxy @(MsgI "K " p))

-- k or prt has access to (Int,a) where Int is the current guard position: hence need to use PrintT
-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out)

