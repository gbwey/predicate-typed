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
-- | extra promoted functions
module Predicate.Data.Lifted (
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

 -- ** monad
  , FFish
  , type (>>=)
  , Sequence
  , Traverse
  , Join

 -- ** alternative
  , type (<|>)

 -- ** comonad
  , Extract
  , Duplicate

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

-- ** error handling
  , Catch
  , Catch'

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import qualified GHC.TypeLits as GL
import Control.Applicative
import Control.Monad (join)
import Data.Kind (Type)
import Control.Comonad (Comonad(duplicate, extract))
import Control.Lens
import Data.Tree (Tree)
import Data.Proxy (Proxy(..))
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Data.Functor.Identity
-- >>> import Data.These
-- >>> :m + Data.Typeable
-- >>> :m + Data.Ratio
-- >>> :m + Control.Lens
-- >>> :m + Control.Lens.Action

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
-- >>> pz @Join (Just (Just 20))
-- Val (Just 20)
--
-- >>> pz @Join ["ab","cd","","ef"]
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

