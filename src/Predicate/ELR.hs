{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
-- | ELR definition
module Predicate.ELR (
 -- definition
    ELR(..)

 -- ** prisms
  , _ENone
  , _ELeft
  , _ERight
  , _EBoth
  , _ELeftish
  , _ERightish

 -- ** isos
  , _elr2Maybe
  , _elr2These

 -- ** boolean predicates
  , isENone
  , isELeft
  , isERight
  , isEBoth

 -- ** type families
  , ENoneT
  , ELeftT
  , ERightT
  , EBothT

 -- ** miscellaneous
  , getBifoldInfo
  , showELR
  , GetELR(..)
  , partitionELR
  , fromELR

 ) where
import Predicate.Misc
import qualified GHC.TypeLits as GL
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import Control.Lens
import Data.Bitraversable
import Data.Bifoldable
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Control.Monad (ap)
import qualified Language.Haskell.TH.Lift as TH
import Data.These
-- $setup
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG

-- | similar to 'Data.These' with an additional empty constructor to support a Monoid instance
data ELR a b =
     ENone -- ^ empty constructor
   | ELeft !a  -- ^ similar to 'Data.These.This'
   | ERight !b -- ^ similar to 'Data.These.That'
   | EBoth !a !b -- ^ similar to 'Data.These.These'
   deriving stock (Show,Eq,Ord,Foldable,Functor,Traversable,Generic,TH.Lift)
   deriving anyclass NFData

makePrisms ''ELR

instance (Semigroup a, Semigroup b) => Semigroup (ELR a b) where
  ENone <> x' = x'
  x <> ENone = x
  ELeft a <> ELeft a' = ELeft (a <> a')
  ELeft a <> ERight b' = EBoth a b'
  ELeft a <> EBoth a' b' = EBoth (a <> a') b'
  ERight b <> ELeft a' = EBoth a' b
  ERight b <> ERight b' = ERight (b <> b')
  ERight b <> EBoth a' b' = EBoth a' (b <> b')
  EBoth a b <> ELeft a' = EBoth (a <> a') b
  EBoth a b <> ERight b' = EBoth a (b <> b')
  EBoth a b <> EBoth a' b' = EBoth (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (ELR a b) where
  mempty = ENone

instance Semigroup x => Applicative (ELR x) where
  pure = ERight
  (<*>) = ap

instance Semigroup x => Monad (ELR x) where
  return = pure
  ENone >>= _ = ENone
  ELeft x >>= _ = ELeft x
  ERight a >>= amb = amb a
  EBoth x a >>= amb =
    case amb a of
      ENone -> ELeft x
      ELeft y -> ELeft (x <> y)
      ERight b -> EBoth x b
      EBoth y b -> EBoth (x <> y) b

instance Bifunctor ELR where
  bimap f g =
    \case
      ENone -> ENone
      ELeft a -> ELeft (f a)
      ERight b -> ERight (g b)
      EBoth a b -> EBoth (f a) (g b)

instance Bifoldable ELR where
  bifoldMap f g =
    \case
      ENone -> mempty
      ELeft a -> f a
      ERight b -> g b
      EBoth a b -> f a <> g b

instance Bitraversable ELR where
  bitraverse f g =
    \case
      ENone -> pure ENone
      ELeft a -> ELeft <$> f a
      ERight b -> ERight <$> g b
      EBoth a b -> EBoth <$> f a <*> g b

-- | display constructor name for 'ELR'
showELR :: ELR a b -> String
showELR = \case
  ENone -> "ENone"
  ELeft {} -> "ELeft"
  ERight {} -> "ERight"
  EBoth {} -> "EBoth"

-- | get 'ELR' from typelevel [type application order is a b then th if explicit kind for th else is first parameter!
class GetELR (th :: ELR k k1) where
  getELR :: (String, ELR w v -> Bool)
instance GetELR 'ENone where
  getELR = ("ENone", isENone)
instance GetELR ('ELeft x) where
  getELR = ("ELeft", isELeft)
instance GetELR ('ERight y) where
  getELR = ("ERight", isERight)
instance GetELR ('EBoth x y) where
  getELR = ("EBoth", isEBoth)

isENone, isELeft, isERight, isEBoth :: ELR a b -> Bool
-- | predicate on ENone
isENone ENone = True
isENone _ = False

-- | predicate on ELeft
isELeft ELeft {} = True
isELeft _ = False

-- | predicate on ERight
isERight ERight {} = True
isERight _ = False

-- | predicate on EBoth
isEBoth EBoth {} = True
isEBoth _ = False

type family ENoneT lr where
  ENoneT (ELR _ _) = ()
  ENoneT o = GL.TypeError (
      'GL.Text "ENoneT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ELeftT lr where
  ELeftT (ELR a _) = a
  ELeftT o = GL.TypeError (
      'GL.Text "ELeftT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ERightT lr where
  ERightT (ELR _ b) = b
  ERightT o = GL.TypeError (
      'GL.Text "ERightT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family EBothT lr where
  EBothT (ELR a b) = (a,b)
  EBothT o = GL.TypeError (
      'GL.Text "EBothT: expected 'ELR a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | partition ELR into 4 lists for each constructor: foldMap (yep ...)
partitionELR :: [ELR a b] -> ([()], [a], [b], [(a,b)])
partitionELR = foldMapStrict $
  \case
    ENone -> ([()],[],[],[])
    ELeft a -> ([],[a],[],[])
    ERight b -> ([],[],[b],[])
    EBoth a b -> ([],[],[],[(a,b)])

-- | convert ELR to a tuple with default values
fromELR :: a -> b -> ELR a b -> (a,b)
fromELR a b =
  \case
    ENone -> (a,b)
    ELeft v -> (v,b)
    ERight w -> (a,w)
    EBoth v w -> (v,w)

-- | iso from 'ELR' to 'These'
--
-- >>> ENone & _elr2These .~ Just (This 12)
-- ELeft 12
--
-- >>> ELeft 123 & _elr2These %~ fmap swapC
-- ERight 123
--
_elr2These :: Iso (ELR a b) (ELR a' b') (Maybe (These a b)) (Maybe (These a' b'))
_elr2These = iso fw bw
  where
  fw = \case
         ENone -> Nothing
         ELeft a -> Just (This a)
         ERight b -> Just (That b)
         EBoth a b -> Just (These a b)
  bw = \case
         Nothing -> ENone
         Just (This a) -> ELeft a
         Just (That b) -> ERight b
         Just (These a b) -> EBoth a b

-- | prism for 'ELeft' that accesses any left value
--
-- >>> ERight 123 ^? _ELeftish
-- Nothing
--
-- >>> ELeft 123 ^? _ELeftish
-- Just 123
--
-- >>> EBoth "Abc" 123 ^? _ELeftish
-- Just "Abc"
--
-- >>> ENone ^? _ELeftish
-- Nothing
--
_ELeftish :: Prism (ELR a x) (ELR a' x) a a'
_ELeftish = prism ELeft
             $ \case
                  ENone -> Left ENone
                  ERight b -> Left (ERight b)
                  ELeft a -> Right a
                  EBoth a _ -> Right a

-- | prism for 'ERight' that accesses any right value
--
-- >>> ELeft 123 ^? _ERightish
-- Nothing
--
-- >>> ERight 123 ^? _ERightish
-- Just 123
--
-- >>> EBoth "Abc" 123 ^? _ERightish
-- Just 123
--
-- >>> ENone ^? _ERightish
-- Nothing
--
_ERightish :: Prism (ELR x b) (ELR x b') b b'
_ERightish = prism ERight
             $ \case
                  ENone -> Left ENone
                  ELeft a -> Left (ELeft a)
                  ERight b -> Right b
                  EBoth _ b -> Right b

-- | iso from 'ELR' to a pair of 'Maybe's
_elr2Maybe :: Iso (ELR a b) (ELR a' b') (Maybe a, Maybe b) (Maybe a', Maybe b')
_elr2Maybe = iso fw bw
  where
  fw = \case
          ENone -> (Nothing, Nothing)
          ELeft a -> (Just a, Nothing)
          ERight b -> (Nothing, Just b)
          EBoth a b -> (Just a, Just b)
  bw = \case
          (Nothing, Nothing) -> ENone
          (Just a, Nothing) -> ELeft a
          (Nothing, Just b) -> ERight b
          (Just a, Just b) -> EBoth a b

-- | 'GetLen' instances for 'ELR'
instance GetLen 'ENone where
  getLen = 0
instance GetLen ('ELeft a) where
  getLen = 0
instance GetLen ('ERight b) where
  getLen = 1
instance GetLen ('EBoth a b) where
  getLen = 1

-- | 'AssocC' instances for 'ELR'
instance AssocC ELR where
  assoc ENone = ENone
  assoc (ELeft ENone) = ENone
  assoc (EBoth ENone _) = ENone
  assoc (ELeft (ELeft a)) = ELeft a
  assoc (ELeft (ERight b)) = ERight (ELeft b)
  assoc (ERight b) = ERight (ERight b)
  assoc (EBoth (ELeft a) c) = EBoth a (ERight c)
  assoc (EBoth (ERight b) c) = ERight (EBoth b c)
  assoc (EBoth (EBoth a b) c) = EBoth a (EBoth b c)
  assoc (ELeft (EBoth a b)) = EBoth a (ELeft b)

  unassoc ENone = ENone
  unassoc (ERight ENone) = ENone
  unassoc (EBoth _ ENone) = ENone

  unassoc (ELeft a) = ELeft (ELeft a)
  unassoc (ERight (ELeft b)) = ELeft (ERight b)
  unassoc (ERight (ERight b)) = ERight b
  unassoc (EBoth a (ERight c)) = EBoth (ELeft a) c
  unassoc (ERight (EBoth b c)) = EBoth (ERight b) c
  unassoc (EBoth a (EBoth b c)) = EBoth (EBoth a b) c
  unassoc (EBoth a (ELeft b)) = ELeft (EBoth a b)

instance SwapC ELR where
  swapC =
    \case
      ENone -> ENone
      ELeft a -> ERight a
      ERight b -> ELeft b
      EBoth a b -> EBoth b a

-- | returns the filled status of a Bifoldable container
getBifoldInfo :: Bifoldable bi => bi a b -> String
getBifoldInfo bi =
  case bifoldMap (const (ELeft ())) (const (ERight ())) bi of
    ENone -> " <skipped>"
    ELeft () -> "(L)"
    ERight () -> "(R)"
    EBoth () () -> "(B)"

