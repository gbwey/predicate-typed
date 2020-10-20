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
-- | promoted functions for proxies
module Predicate.Data.Proxy (
 -- ** proxy functions
    Proxify
  , Pop0
  -- *** require kind signatures on @p@
  , Pop1
  , Pop2
  , Pop1'
  , Pop2'
  , PApp
  , PApp2
 -- *** proxy type families
  , Pop0T
  , Pop1T
  , Pop2T
  , Pop1'T
  , Pop2'T
  , PAppT
  , PApp2T

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import qualified GHC.TypeLits as GL
import Data.Kind (Type)
import Data.Typeable
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> import Predicate.Prelude
-- >>> import Control.Lens
-- >>> import Control.Lens.Action
-- >>> import qualified Data.Semigroup as SG
-- >>> :m + Data.Ratio

-- | run the proxy @p@ in the environment pointed to by @q@
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
type family Pop0T (p :: Type) (q :: Type) :: Type where
  Pop0T (Proxy z) q = PP z q
  Pop0T (_proxy z) q = PP z q
  Pop0T p q = GL.TypeError (
     'GL.Text "Pop0T: requires 'Proxy z' and 'q' get applied to each other"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
   )

-- | applies Proxy @p@ to @q@ in the environment pointed to by @r@ : needs kind signatures on @p@
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

type family Pop1T (p :: Type) (q :: k) (r :: Type) :: Type where
  Pop1T (Proxy z) q r = PP (z q) r
--  Pop1T (Proxy (z :: k -> k1)) (q :: k) r = PP (z q :: k1) r
  Pop1T p q r =
    GL.TypeError (
     'GL.Text "Pop1T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
    )

-- | apply Proxy @p@ to Proxy @q@ and run in the environment pointed to by @r@ : needs kind signatures on @p@
--
-- >>> pz @(Pop1' (Proxy ((<>) Snd)) (Proxy Fst) Id) ("abc","def")
-- Val "defabc"
--
-- >>> pz @(Pop1' (Proxy ((>>) Snd)) (Proxy (Resplit "\\." >> Map (ReadP Int Id))) Id) (1,"123.33.5")
-- Val [123,33,5]
--
-- >>> pz @(Pop1' (Proxy (Lift Snd)) (Proxy Fst) Id) ((True,2),("abc",1 % 4))
-- Val 2
--
-- >>> pz @(Pop1' Fst Snd Thd) (Proxy @(Lift Snd), Proxy @Fst,((True,2),("abc",1 % 4)))
-- Val 2
--
-- >>> pz @(Pop1' Fst Snd '( '( 'True,2),'("abc",1 % 4))) (Proxy @(Lift Snd), Proxy @Fst)
-- Val 2
--
-- >>> pz @(Pop1' (Proxy MEmptyT) (Proxy (SG.Sum _)) ()) ()
-- Val (Sum {getSum = 0})
--
-- >>> pz @(Pop1' (Proxy MEmptyT) (PApp (Proxy SG.Sum) (Proxy Float)) ()) ()
-- Val (Sum {getSum = 0.0})
--
data Pop1' p q r deriving Show

instance ( P r x
         , PP p x ~ Proxy (z :: k -> k1)
         , PP q x ~ Proxy (w :: k)
         , P (z w) (PP r x)
         ) => P (Pop1' p q r) x where
  type PP (Pop1' p q r) x = Pop1'T (PP p x) (PP q x) (PP r x)
  eval _ opts x = do
    let msg0 = "Pop1'"
    rr <- eval (Proxy @r) opts x
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right r -> do
        zz <- eval (Proxy @(z w)) opts r
        case getValueLR NoInline opts msg0 zz [hh rr] of
          Left e -> pure e
          Right _z -> return $ mkNodeCopy opts zz (msg0 <> nullIf " | " (_ttString zz)) [hh rr,hh zz]

type family Pop1'T (p :: Type) (q :: Type) (r :: Type) :: Type where
  Pop1'T (Proxy z) (Proxy w) r = PP (z w) r
--  Pop1'T (Proxy (z :: k -> k1)) (Proxy (w :: k)) r = PP (z w :: k1) r
  Pop1'T p q r =
    GL.TypeError (
     'GL.Text "Pop1'T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
    )

-- | apply Proxy @p@ to @q@ and @r@ then run in the environment pointed to by @s@ : needs kind signatures on @p@
--
-- >>> pz @(Pop2 (Proxy '(,)) Fst 14 Id) ([1..4],'True)
-- Val ([1,2,3,4],14)
--
-- >>> pz @(Pop2' (Proxy Pure) (Proxy SG.Sum) (Proxy Id) Id) 123
-- Val (Sum {getSum = 123})
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
type family Pop2T (p :: Type) (q :: k) (r :: k1) (s :: Type) :: Type where
  Pop2T (Proxy z) q r s = PP (z q r) s
--  Pop2T (Proxy (z :: k -> k1 -> k2)) (q :: k) (r :: k1) s = PP (z q r :: k2) s
  Pop2T p q r s =
    GL.TypeError (
     'GL.Text "Pop2T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
       'GL.:$$: 'GL.Text " s = " 'GL.:<>: 'GL.ShowType s
    )

-- | Applies Proxy @p@ to Proxy @q@ and Proxy @r@ and runs in the environment pointed to by @s@ : needs kind signatures on @p@
--
-- >>> pz @(Pop2' (Proxy '(,)) (Proxy 1) (Proxy "sss") ()) ()
-- Val (1,"sss")
--
-- >>> pz @(Pop2' (Proxy '(,)) (Proxy L31) (Proxy (Fst % Snd)) '(11,99,'("ss",3))) ()
-- Val ("ss",1 % 9)
--
-- >>> pz @(Pop2' Fst Snd Thd (L4 Id)) (Proxy @'(,), Proxy @L31, Proxy @(Fst % Snd), (11,99,("ss",3)))
-- Val ("ss",1 % 9)
--
data Pop2' p q r s deriving Show

instance ( P s x
         , PP p x ~ Proxy (z :: k -> k1 -> k2)
         , PP q x ~ Proxy (w :: k)
         , PP r x ~ Proxy (v :: k1)
         , P (z w v) (PP s x)
         ) => P (Pop2' p q r s) x where
  type PP (Pop2' p q r s) x = Pop2'T (PP p x) (PP q x) (PP r x) (PP s x)
  eval _ opts x = do
    let msg0 = "Pop2'"
    ss <- eval (Proxy @s) opts x
    case getValueLR NoInline opts msg0 ss [] of
      Left e -> pure e
      Right s -> do
        zz <- eval (Proxy @(z w v)) opts s
        case getValueLR NoInline opts msg0 zz [hh ss] of
          Left e -> pure e
          Right _z -> return $ mkNodeCopy opts zz (msg0 <> nullIf " | " (_ttString zz)) [hh ss,hh zz]

-- pass all the arguments in!!! else ghc gets confused
type family Pop2'T (p :: Type) (q :: Type) (r :: Type) (s :: Type) :: Type where
  Pop2'T (Proxy z) (Proxy w) (Proxy v) s = PP (z w v) s
--  Pop2'T (Proxy (z :: k -> k1 -> k2)) (Proxy (w :: k)) (Proxy (v :: k1)) s = PP (z w v :: k2) s
  Pop2'T p q r s =
    GL.TypeError (
     'GL.Text "Pop2'T: requires 'Proxy z' and z must be a function requiring one parameter!!"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
       'GL.:$$: 'GL.Text " s = " 'GL.:<>: 'GL.ShowType s
    )

-- | applies Proxy @p@ to Proxy @q@ and returns a Proxy: needs kind signatures on @p@
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
-- >>> pz @(PApp (PApp (Proxy ('(,) :: GL.Nat -> GL.Symbol -> (GL.Nat,GL.Symbol))) (Proxy 1)) (Proxy "abc")) () ^!? acts . _Val . to typeRep
-- Just ('(,) Nat Symbol 1 "abc")
--
data PApp p q deriving Show

instance ( PP p x ~ Proxy (z :: k -> k1)
         , PP q x ~ Proxy (w :: k)
         ) => P (PApp p q) x where
  type PP (PApp p q) x = PAppT (PP p x) (PP q x)
  eval _ opts _ =
    pure $ mkNode opts (Val Proxy) "PApp" []

type family PAppT (p :: Type) (q :: Type) :: Type where
  PAppT (Proxy z) (Proxy w) = Proxy (z w)
--  PAppT (Proxy (z :: k -> k1)) (Proxy (w :: k)) = Proxy (z w :: k1)
  PAppT p q =
    GL.TypeError (
     'GL.Text "PAppT: requires 'Proxy z' and 'Proxy w' get applied to each other"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
    )

-- | applies Proxy @p@ to Proxy @q@ and Proxy @r@ and returns a Proxy: needs kind signatures on @p@
-- | applicative bind of proxies p, q, and r: Proxy (z :: k -> k1 -> k2) <*> Proxy (w :: k) <*> Proxy (v :: k1) = Proxy (u :: k2) : needs kind signatures on @p@
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
  type PP (PApp2 p q r) x = PApp2T (PP p x) (PP q x) (PP r x)
  eval _ opts _ =
    pure $ mkNode opts (Val Proxy) "PApp2" []

type family PApp2T (p :: Type) (q :: Type) (r :: Type) :: Type where
  PApp2T (Proxy z) (Proxy w) (Proxy v) = Proxy (z w v)
  --PApp2T (Proxy (z :: k -> k1 -> k2)) (Proxy (w :: k)) (Proxy (v :: k1)) = Proxy (z w v :: k2)
  PApp2T p q r =
    GL.TypeError (
     'GL.Text "PApp2T: requires 'Proxy z', 'Proxy w' and 'Proxy v': z is applied to w and v"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
       'GL.:$$: 'GL.Text " q = " 'GL.:<>: 'GL.ShowType q
       'GL.:$$: 'GL.Text " r = " 'GL.:<>: 'GL.ShowType r
   )

-- | create a Proxy z from proxy z where z is the expression pointed to by @p@ : Proxify alway returns Val (Proxy @z)
--
-- >>> pz @(Proxify Fst) ([True],13) ^!? acts . _Val . only (Proxy @Bool)
-- Just ()
--
-- >>> pz @(Proxify (MkJust 1)) () ^!? acts . _Val . to typeRep
-- Just Int
--
-- >>> pz @(Proxify (FailT [Double] "abc")) () ^!? acts . _Val . to typeRep
-- Just Double
--
-- >>> pz @(Proxify "abc") () ^!? acts . _Val . to typeRep
-- Just Char
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Double) ^!? acts . ttVal . _Val . to typeRep
-- Just Double
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^? _Id . ttVal . _Val == Just (Proxy @Int)
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^? _Wrapped @(Identity _) . ttVal . _Val == Just (Proxy @Int)
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts (Nothing @Double) ^. to runIdentity . ttVal . singular _Val == Proxy @Double
-- True
--
-- >>> eval (Proxy @(Proxify Id)) defOpts ([] @Int) ^? folded @Identity . ttVal . _Val == Just (Proxy @Int)
-- True
data Proxify p
instance PP p x ~ proxy (z :: k)
      => P (Proxify p) x where
  type PP (Proxify p) x = ProxifyT (PP p x)
  eval _ opts _ =
    pure $ mkNode opts (Val Proxy) "Proxify" []

type family ProxifyT p where
  ProxifyT (_proxy z) = Proxy z
  ProxifyT p = GL.TypeError (
     'GL.Text "ProxifyT: requires any 'proxy z'"
       'GL.:$$: 'GL.Text " p = " 'GL.:<>: 'GL.ShowType p
      )
