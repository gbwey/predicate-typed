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
-- | promoted 'Either' functions
module Predicate.Data.Either (

 -- ** boolean predicates
    IsLeft
  , IsRight

 -- ** constructors
  , MkLeft
  , MkLeft'
  , MkRight
  , MkRight'

 -- ** get rid of Either
  , Left'
  , Right'
  , LeftDef
  , LeftFail
  , RightDef
  , RightFail
  , EitherBool
  , EitherIn
  , PartitionEithers

 -- ** miscellaneous
  , type (|||)
  , type (+++)

 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)
import Data.Either (isLeft, isRight, partitionEithers)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | extracts the left value from an 'Either'
--
-- >>> pz @(Left' >> Succ) (Left 20)
-- Val 21
--
-- >>> pz @(Left' >> Succ) (Right 'a')
-- Fail "Left' found Right"
--
data Left' deriving Show
instance Show a => P Left' (Either a x) where
  type PP Left' (Either a x) = a
  eval _ opts lr =
    let msg0 = "Left'"
    in pure $ case lr of
         Right _ -> mkNode opts (Fail (msg0 <> " found Right")) "" []
         Left a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | extracts the right value from an 'Either'
--
-- >>> pz @(Right' >> Succ) (Right 20)
-- Val 21
--
-- >>> pz @(Right' >> Succ) (Left 'a')
-- Fail "Right' found Left"
--
data Right' deriving Show
instance Show a => P Right' (Either x a) where
  type PP Right' (Either x a) = a
  eval _ opts lr =
    let msg0 = "Right'"
    in pure $ case lr of
         Left _ -> mkNode opts (Fail (msg0 <> " found Left")) "" []
         Right a -> mkNode opts (Val a) (msg0 <> " " <> showL opts a) []

-- | similar 'Control.Arrow.|||'
--
-- >>> pz @(Pred ||| Id) (Left 13)
-- Val 12
--
-- >>> pz @(ShowP Id ||| Id) (Right "hello")
-- Val "hello"
--
-- >>> pl @('True ||| 'False) (Left "someval")
-- True ((|||) Left True | "someval")
-- Val True
--
-- >>> pl @('True ||| 'False) (Right "someval")
-- False ((|||) Right False | "someval")
-- Val False
--
-- >>> pl @(ShowP Succ ||| ShowP Id) (Left 123)
-- Present "124" ((|||) Left "124" | 123)
-- Val "124"
--
-- >>> pl @(ShowP Succ ||| ShowP Id) (Right True)
-- Present "True" ((|||) Right "True" | True)
-- Val "True"
--
-- >>> pl @(EitherIn (Not Id) Id) (Right True)
-- Present True ((|||) Right True | True)
-- Val True
--
-- >>> pl @(EitherIn (Not Id) Id) (Left True)
-- False ((|||) Left False | True)
-- Val False
--
data p ||| q deriving Show
infixr 2 |||
type EitherIn p q = p ||| q

instance ( Show (PP p a)
         , P p a
         , P q b
         , PP p a ~ PP q b
         , Show a
         , Show b
         ) => P (p ||| q) (Either a b) where
  type PP (p ||| q) (Either a b) = PP p a
  eval _ opts lr = do
    let msg0 = "(|||)"
    case lr of
      Left a -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR NoInline opts msg0 pp [] of
          Left e -> e
          Right a1 -> let msg1 = msg0 ++ " Left"
                      in mkNodeCopy opts pp (show3 opts msg1 a1 a) [hh pp]
      Right a -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg0 qq [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Right"
            in mkNodeCopy opts qq (show3 opts msg1 a1 a) [hh qq]

-- | similar to 'isLeft'
--
-- >>> pz @IsLeft (Right 123)
-- Val False
--
-- >>> pz @IsLeft (Left 'a')
-- Val True
--
data IsLeft deriving Show

instance x ~ Either a b
       => P IsLeft x where
  type PP IsLeft x = Bool
  eval _ opts x = pure $ mkNodeB opts (isLeft x) "IsLeft" []

-- | similar to 'isRight'
--
-- >>> pz @IsRight (Right 123)
-- Val True
--
-- >>> pz @IsRight (Left "aa")
-- Val False
--
data IsRight deriving Show

instance x ~ Either a b
         => P IsRight x where
  type PP IsRight x = Bool
  eval _ opts x = pure $ mkNodeB opts (isRight x) "IsRight" []


-- | similar 'Control.Arrow.+++'
--
-- >>> pz @(Pred +++ Id) (Left 13)
-- Val (Left 12)
--
-- >>> pz @(ShowP Id +++ Reverse) (Right "hello")
-- Val (Right "olleh")
--
-- >>> pl @(HeadDef 'False Id +++ Id) (Right @[Bool] 1) -- need @[Bool] cos we said 'False!
-- Present Right 1 ((+++) Right 1 | 1)
-- Val (Right 1)
--
-- >>> pl @(HeadDef 'False Id +++ Id) (Left [True,False]) -- need @[Bool] cos we said 'False!
-- Present Left True ((+++) Left True | [True,False])
-- Val (Left True)
--
-- >>> pl @(Not Id +++ Id) (Right True)
-- Present Right True ((+++) Right True | True)
-- Val (Right True)
--
-- >>> pl @(Not Id +++ Id) (Right 12)
-- Present Right 12 ((+++) Right 12 | 12)
-- Val (Right 12)
--
-- >>> pl @(HeadDef () Id +++ Id) (Right @[()] 1) -- breaks otherwise: Id says () -> () so has to be a list of [()]
-- Present Right 1 ((+++) Right 1 | 1)
-- Val (Right 1)
--
-- >>> pl @(HeadDef () Id +++ Id) (Right @[()] 1) -- this breaks! cos Left doesnt have a type
-- Present Right 1 ((+++) Right 1 | 1)
-- Val (Right 1)
--
-- >>> pl @(Not Id +++ Id) (Right @Bool 12)
-- Present Right 12 ((+++) Right 12 | 12)
-- Val (Right 12)
--
data p +++ q deriving Show
infixr 2 +++

instance ( Show (PP p a)
         , Show (PP q b)
         , P p a
         , P q b
         , Show a
         , Show b
         ) => P (p +++ q) (Either a b) where
  type PP (p +++ q) (Either a b) = Either (PP p a) (PP q b)
  eval _ opts lr = do
    let msg0 = "(+++)"
    case lr of
      Left a -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR NoInline opts msg0 pp [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Left"
            in mkNode opts (Val (Left a1)) (msg1 <> " " <> showL opts a1 <> showVerbose opts " | " a) [hh pp]
      Right a -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR NoInline opts msg0 qq [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Right"
            in mkNode opts (Val (Right a1)) (msg1 <> " " <> showL opts a1 <> showVerbose opts " | " a) [hh qq]

-- | similar to 'partitionEithers'
--
-- >>> pz @PartitionEithers [Left 'a',Right 2,Left 'c',Right 4,Right 99]
-- Val ("ac",[2,4,99])
--
-- >>> pz @PartitionEithers [Right 2,Right 4,Right 99]
-- Val ([],[2,4,99])
--
-- >>> pz @PartitionEithers [Left 'a',Left 'c']
-- Val ("ac",[])
--
-- >>> pz @PartitionEithers ([] :: [Either () Int])
-- Val ([],[])
--
-- >>> pl @PartitionEithers [Left 4, Right 'x', Right 'y',Left 99]
-- Present ([4,99],"xy") (PartitionEithers ([4,99],"xy") | [Left 4,Right 'x',Right 'y',Left 99])
-- Val ([4,99],"xy")
--
-- >>> pl @PartitionEithers [Left 'x', Right 1,Left 'a', Left 'b',Left 'z', Right 10]
-- Present ("xabz",[1,10]) (PartitionEithers ("xabz",[1,10]) | [Left 'x',Right 1,Left 'a',Left 'b',Left 'z',Right 10])
-- Val ("xabz",[1,10])
--
data PartitionEithers deriving Show

instance ( Show a
         , Show b
         ) => P PartitionEithers [Either a b] where
  type PP PartitionEithers [Either a b] = ([a], [b])
  eval _ opts as =
    let msg0 = "PartitionEithers"
        b = partitionEithers as
    in pure $ mkNode opts (Val b) (show3 opts msg0 b as) []

-- | Convenient method to convert a @p@ or @q@ to a 'Either' based on a predicate @b@
--   if @b@ then Right @p@ else Left @q@
--
-- >>> pz @(EitherBool (Fst > 4) L21 L22) (24,(-1,999))
-- Val (Right 999)
--
-- >>> pz @(EitherBool (Fst > 4) L21 L22) (1,(-1,999))
-- Val (Left (-1))
--
-- >>> pl @(EitherBool (Fst > 10) L21 L22) (7,('x',99))
-- Present Left 'x' (EitherBool(False) Left 'x')
-- Val (Left 'x')
--
-- >>> pl @(EitherBool (Fst > 10) L21 L22) (11,('x',99))
-- Present Right 99 (EitherBool(True) Right 99)
-- Val (Right 99)
--
-- >>> pl @(EitherBool (Gt 10) "found left" 99) 12
-- Present Right 99 (EitherBool(True) Right 99)
-- Val (Right 99)
--
-- >>> pl @(EitherBool (Gt 10) "found left" 99) 7
-- Present Left "found left" (EitherBool(False) Left "found left")
-- Val (Left "found left")
--
data EitherBool b p q deriving Show

instance ( Show (PP p a)
         , P p a
         , Show (PP q a)
         , P q a
         , P b a
         , PP b a ~ Bool
         ) => P (EitherBool b p q) a where
  type PP (EitherBool b p q) a = Either (PP p a) (PP q a)
  eval _ opts z = do
    let msg0 = "EitherBool"
    bb <- evalBool (Proxy @b) opts z
    case getValueLR NoInline opts (msg0 <> " b failed") bb [] of
      Left e -> pure e
      Right False -> do
        pp <- eval (Proxy @p) opts z
        pure $ case getValueLR NoInline opts (msg0 <> " p failed") pp [hh bb] of
          Left e -> e
          Right p -> mkNode opts (Val (Left p)) (msg0 <> "(False) Left " <> showL opts p) [hh bb, hh pp]
      Right True -> do
        qq <- eval (Proxy @q) opts z
        pure $ case getValueLR NoInline opts (msg0 <> " q failed") qq [hh bb] of
          Left e -> e
          Right q -> mkNode opts (Val (Right q)) (msg0 <> "(True) Right " <> showL opts q) [hh bb, hh qq]

-- | similar to 'Control.Arrow.|||' but additionally gives @p@ and @q@ the original input
--
-- >>> pz @(EitherX (ShowP (L11 + Snd)) (ShowP Id) Snd) (9,Left 123)
-- Val "132"
--
-- >>> pz @(EitherX (ShowP (L11 + Snd)) (ShowP Id) Snd) (9,Right 'x')
-- Val "((9,Right 'x'),'x')"
--
-- >>> pz @(EitherX (ShowP Id) (ShowP (Second Succ)) Snd) (9,Right 'x')
-- Val "((9,Right 'x'),'y')"
--
data EitherX p q r deriving Show
instance ( P r x
         , P p (x,a)
         , P q (x,b)
         , PP r x ~ Either a b
         , PP p (x,a) ~ c
         , PP q (x,b) ~ c
         ) => P (EitherX p q r) x where
  type PP (EitherX p q r) x = EitherXT (PP r x) x p
  eval _ opts x = do
    let msg0 = "EitherX"
    rr <- eval (Proxy @r) opts x
    case getValueLR NoInline opts msg0 rr [] of
      Left e -> pure e
      Right (Left a) -> do
        let msg1 = msg0 <> "(Left)"
        pp <- eval (Proxy @p) opts (x,a)
        pure $ case getValueLR NoInline opts msg1 pp [hh rr] of
          Left e -> e
          Right _ -> mkNodeCopy opts pp msg1 [hh rr, hh pp]
      Right (Right b) -> do
        let msg1 = msg0 <> "(Right)"
        qq <- eval (Proxy @q) opts (x,b)
        pure $ case getValueLR NoInline opts msg1 qq [hh rr] of
          Left e -> e
          Right _ -> mkNodeCopy opts qq msg1 [hh rr, hh qq]

type family EitherXT lr x p where
  EitherXT (Either a _b) x p = PP p (x,a)
  EitherXT o _ _ = GL.TypeError (
      'GL.Text "EitherXT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | 'Data.Either.Left' constructor
data MkLeft' t p deriving Show

instance ( Show (PP p x)
         , P p x
         ) => P (MkLeft' t p) x where
  type PP (MkLeft' t p) x = Either (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkLeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Left p
        in mkNode opts (Val d) (msg0 <> " Left " <> showL opts p) [hh pp]

-- | 'Data.Either.Left' constructor
--
-- >>> pz @(MkLeft _ Id) 44
-- Val (Left 44)
--
data MkLeft (t :: Type) p deriving Show
type MkLeftT (t :: Type) p = MkLeft' (Hole t) p

instance P (MkLeftT t p) x => P (MkLeft t p) x where
  type PP (MkLeft t p) x = PP (MkLeftT t p) x
  eval _ = eval (Proxy @(MkLeftT t p))

-- | 'Data.Either.Right' constructor
data MkRight' t p deriving Show

instance ( Show (PP p x)
         , P p x
         ) => P (MkRight' t p) x where
  type PP (MkRight' t p) x = Either (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkRight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Right p
        in mkNode opts (Val d) (msg0 <> " Right " <> showL opts p) [hh pp]

-- | 'Data.Either.Right' constructor
--
-- >>> pz @(MkRight _ Id) 44
-- Val (Right 44)
--
data MkRight (t :: Type) p deriving Show
type MkRightT (t :: Type) p = MkRight' (Hole t) p

instance P (MkRightT t p) x => P (MkRight t p) x where
  type PP (MkRight t p) x = PP (MkRightT t p) x
  eval _ = eval (Proxy @(MkRightT t p))

-- | extract the Left value from an 'Either' otherwise use the default value: similar to 'Data.Either.fromLeft'
--
-- if there is no Left value then \p\ is passed the Right value and the whole context
--
-- >>> pz @(LeftDef (1 % 4) Id) (Left 20.4)
-- Val (102 % 5)
--
-- >>> pz @(LeftDef (1 % 4) Id) (Right "aa")
-- Val (1 % 4)
--
-- >>> pz @(LeftDef (PrintT "found right=%s fst=%d" '(Fst,L21)) Snd) (123,Right "xy")
-- Val "found right=xy fst=123"
--
-- >>> pz @(LeftDef (MEmptyT _) Id) (Right 222)
-- Val ()
--
-- >>> pz @(LeftDef (MEmptyT (SG.Sum _)) Id) (Right 222)
-- Val (Sum {getSum = 0})
--
data LeftDef p q deriving Show

instance ( PP q x ~ Either a b
         , PP p (b,x) ~ a
         , P q x
         , P p (b,x)
    ) => P (LeftDef p q) x where
  type PP (LeftDef p q) x = LeftT (PP q x)
  eval _ opts x = do
    let msg0 = "LeftDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Left a -> pure $ mkNode opts (Val a) (msg0 <> " Left") [hh qq]
          Right b -> do
            pp <- eval (Proxy @p) opts (b,x)
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Val p) (msg0 <> " Right") [hh qq, hh pp]

-- | extract the Right value from an 'Either': similar to 'Data.Either.fromRight'
--
-- if there is no Right value then \p\ is passed the Left value and the whole context
--
-- >>> pz @(RightDef (1 % 4) Id) (Right 20.4)
-- Val (102 % 5)
--
-- >>> pz @(RightDef (1 % 4) Id) (Left "aa")
-- Val (1 % 4)
--
-- >>> pz @(RightDef (PrintT "found left=%s fst=%d" '(Fst,L21)) Snd) (123,Left "xy")
-- Val "found left=xy fst=123"
--
-- >>> pz @(RightDef (MEmptyT _) Id) (Left 222)
-- Val ()
--
-- >>> pz @(RightDef (MEmptyT (SG.Sum _)) Id) (Left 222)
-- Val (Sum {getSum = 0})
--
data RightDef p q deriving Show

instance ( PP q x ~ Either a b
         , PP p (a,x) ~ b
         , P q x
         , P p (a,x)
    ) => P (RightDef p q) x where
  type PP (RightDef p q) x = RightT (PP q x)
  eval _ opts x = do
    let msg0 = "RightDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Right b -> pure $ mkNode opts (Val b) (msg0 <> " Right") [hh qq]
          Left a -> do
            pp <- eval (Proxy @p) opts (a,x)
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Val p) (msg0 <> " Left") [hh qq, hh pp]


-- | extract the Left value from an 'Either' otherwise fail with a message
--
-- if there is no Left value then \p\ is passed the Right value and the whole context
--
-- >>> pz @(LeftFail "oops" Id) (Left 20.4)
-- Val 20.4
--
-- >>> pz @(LeftFail "oops" Id) (Right "aa")
-- Fail "oops"
--
-- >>> pz @(LeftFail (PrintT "found right=%s fst=%d" '(Fst,L21)) Snd) (123,Right "xy")
-- Fail "found right=xy fst=123"
--
-- >>> pz @(LeftFail (MEmptyT _) Id) (Right 222)
-- Fail ""
--
-- >>> pl @(LeftFail (PrintF "someval=%d" L21) Snd) (13::Int,Right @(SG.Sum Int) "abc")
-- Error someval=13 (LeftFail Right)
-- Fail "someval=13"
--
-- >>> pl @(LeftFail (PrintF "someval=%s" Fst) Id) (Right @(SG.Sum Int) "abc")
-- Error someval=abc (LeftFail Right)
-- Fail "someval=abc"
--
-- >>> pl @(LeftFail (PrintF "found rhs=%d" Fst) Id) (Right @String @Int 10)
-- Error found rhs=10 (LeftFail Right)
-- Fail "found rhs=10"
--
-- >>> pl @(LeftFail (PrintF "found rhs=%d" (Snd >> L22)) L21) ('x',(Right 10,23))
-- Error found rhs=23 (LeftFail Right)
-- Fail "found rhs=23"
--
-- >>> pl @(LeftFail (PrintF "found rhs=%d" (L2 L22)) L21) ('x',(Left "abc",23))
-- Present "abc" (Left)
-- Val "abc"
--
data LeftFail p q deriving Show

instance ( PP p (b,x) ~ String
         , PP q x ~ Either a b
         , P p (b,x)
         , P q x
         )
    => P (LeftFail p q) x where
  type PP (LeftFail p q) x = LeftT (PP q x)
  eval _ opts x = do
    let msg0 = "LeftFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Left a -> pure $ mkNode opts (Val a) "Left" [hh qq]
          Right b -> do
            pp <- eval (Proxy @p) opts (b,x)
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " Right") [hh qq, hh pp]


-- | extract the Right value from an 'Either' otherwise fail with a message
--
-- if there is no Right value then \p\ is passed the Left value and the whole context
--
-- >>> pz @(RightFail "oops" Id) (Right 20.4)
-- Val 20.4
--
-- >>> pz @(RightFail "oops" Id) (Left "aa")
-- Fail "oops"
--
-- >>> pz @(RightFail (PrintT "found left=%s fst=%d" '(Fst,L21)) Snd) (123,Left "xy")
-- Fail "found left=xy fst=123"
--
-- >>> pz @(RightFail (MEmptyT _) Id) (Left 222)
-- Fail ""
--
data RightFail p q deriving Show

instance ( PP p (a,x) ~ String
         , PP q x ~ Either a b
         , P p (a,x)
         , P q x
         )
    => P (RightFail p q) x where
  type PP (RightFail p q) x = RightT (PP q x)
  eval _ opts x = do
    let msg0 = "RightFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR NoInline opts msg0 qq [] of
      Left e -> pure e
      Right q ->
        case q of
          Right b -> pure $ mkNode opts (Val b) "Right" [hh qq]
          Left a -> do
            pp <- eval (Proxy @p) opts (a,x)
            pure $ case getValueLR NoInline opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (Fail p) (msg0 <> " Left") [hh qq, hh pp]
