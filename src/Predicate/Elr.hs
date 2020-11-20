{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
-- | Elr definition
module Predicate.Elr (
 -- ** definition
    Elr(..)

 -- ** prisms
  , _ENone
  , _ELeft
  , _ERight
  , _EBoth

 -- ** isos
  , _elr2Maybe
  , _elr2These

 -- ** predicates
  , isENone
  , isELeft
  , isERight
  , isEBoth

 -- ** type families
  , ELeftT
  , ERightT
  , EBothT

 -- ** miscellaneous
  , getBifoldInfo
  , showElr
  , GetElr(..)
  , getElr
  , partitionElr
  , fromElr
  , mergeElrWith
  , elr
 ) where
import Predicate.Misc
import qualified GHC.TypeLits as GL
import GHC.TypeLits (ErrorMessage((:$$:),(:<>:)))
import Control.Lens
import Data.Bitraversable (Bitraversable(..))
import Data.Bifoldable (Bifoldable(bifoldMap))
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq (NFData(..), NFData1(..), NFData2(..), rnf1)
import Control.Monad (ap)
import Data.These (These(..))
import Data.Data (Data)
import qualified Language.Haskell.TH.Syntax as TH
-- $setup
-- >>> import Predicate

-- | combination of values for two types @a@ and @b@
data Elr a b =
     ENone -- ^ no value
   | ELeft a  -- ^ left value
   | ERight b -- ^ right value
   | EBoth a b -- ^ both left and a right value
   deriving stock (Show,Eq,Ord,Foldable,Functor,Traversable,Generic,Generic1,Data)

instance (NFData a, NFData b) => NFData (Elr a b) where
  rnf = rnf1

instance NFData a => NFData1 (Elr a) where
  liftRnf = liftRnf2 rnf

instance NFData2 Elr where
  liftRnf2 _l _r ENone = ()
  liftRnf2 l _r (ELeft a) = l a
  liftRnf2 _l r (ERight b) = r b
  liftRnf2 l r (EBoth a b) = l a `seq` r b

deriving instance (TH.Lift a, TH.Lift b) => TH.Lift (Elr a b)

makePrisms ''Elr

instance (Semigroup a, Semigroup b) => Semigroup (Elr a b) where
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

instance (Monoid a, Monoid b) => Monoid (Elr a b) where
  mempty = ENone

instance Semigroup x => Applicative (Elr x) where
  pure = ERight
  (<*>) = ap

instance Semigroup x => Monad (Elr x) where
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

instance Bifunctor Elr where
  bimap f g =
    \case
      ENone -> ENone
      ELeft a -> ELeft (f a)
      ERight b -> ERight (g b)
      EBoth a b -> EBoth (f a) (g b)

instance Bifoldable Elr where
  bifoldMap f g =
    \case
      ENone -> mempty
      ELeft a -> f a
      ERight b -> g b
      EBoth a b -> f a <> g b

instance Bitraversable Elr where
  bitraverse f g =
    \case
      ENone -> pure ENone
      ELeft a -> ELeft <$> f a
      ERight b -> ERight <$> g b
      EBoth a b -> EBoth <$> f a <*> g b

-- | display constructor name for 'Elr'
showElr :: Elr a b -> String
showElr = \case
  ENone -> "ENone"
  ELeft {} -> "ELeft"
  ERight {} -> "ERight"
  EBoth {} -> "EBoth"

-- | get 'Elr' from typelevel [type application order is a b then th if explicit kind for th else is first parameter!
class GetElr (elr :: Elr k k1) where
  getElr' :: Elr () ()
instance GetElr 'ENone where
  getElr' = ENone
instance GetElr ('ELeft x) where
  getElr' = ELeft ()
instance GetElr ('ERight y) where
  getElr' = ERight ()
instance GetElr ('EBoth x y) where
  getElr' = EBoth () ()

-- | get 'Elr' from the typelevel
getElr :: forall th . GetElr th => Elr () ()
getElr = getElr' @_ @_ @th

isENone, isELeft, isERight, isEBoth :: Elr a b -> Bool
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

-- | extract the type from 'ELeft'
type family ELeftT lr where
  ELeftT (Elr a _) = a
  ELeftT o = GL.TypeError (
      'GL.Text "ELeftT: expected 'Elr a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract the type from 'ERight'
type family ERightT lr where
  ERightT (Elr _ b) = b
  ERightT o = GL.TypeError (
      'GL.Text "ERightT: expected 'Elr a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract the types as a tuple from 'EBoth'
type family EBothT lr where
  EBothT (Elr a b) = (a,b)
  EBothT o = GL.TypeError (
      'GL.Text "EBothT: expected 'Elr a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | partition Elr into 4 lists for each constructor: foldMap (yep ...)
partitionElr :: [Elr a b] -> ([()], [a], [b], [(a,b)])
partitionElr = foldMapStrict $
  \case
    ENone -> ([()],[],[],[])
    ELeft a -> ([],[a],[],[])
    ERight b -> ([],[],[b],[])
    EBoth a b -> ([],[],[],[(a,b)])

-- | convert Elr to a tuple with default values
fromElr :: a -> b -> Elr a b -> (a,b)
fromElr a b =
  \case
    ENone -> (a,b)
    ELeft v -> (v,b)
    ERight w -> (a,w)
    EBoth v w -> (v,w)

-- | iso from 'Elr' to 'These'
--
-- >>> ENone & _elr2These .~ Just (This 12)
-- ELeft 12
--
-- >>> ELeft 123 & _elr2These %~ fmap swapC
-- ERight 123
--
_elr2These :: Iso (Elr a b) (Elr a' b') (Maybe (These a b)) (Maybe (These a' b'))
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

-- | iso from 'Elr' to a 'Maybe' pair
--
-- >>> ENone ^. _elr2Maybe
-- (Nothing,Nothing)
--
-- >>> ELeft 123 ^. _elr2Maybe
-- (Just 123,Nothing)
--
-- >>> EBoth 1 'a' ^. _elr2Maybe
-- (Just 1,Just 'a')
--
_elr2Maybe :: Iso (Elr a b) (Elr a' b') (Maybe a, Maybe b) (Maybe a', Maybe b')
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

instance GetLen 'ENone where
  getLen = 0
instance GetLen ('ELeft a) where
  getLen = 0
instance GetLen ('ERight b) where
  getLen = 1
instance GetLen ('EBoth a b) where
  getLen = 1

instance AssocC Elr where
  assoc ENone = ENone
  assoc (ELeft ENone) = ENone
  assoc (ELeft (ELeft a)) = ELeft a
  assoc (ELeft (ERight b)) = ERight (ELeft b)
  assoc (ELeft (EBoth a b)) = EBoth a (ELeft b)
  assoc (ERight c) = ERight (ERight c)
  assoc (EBoth ENone c) = ERight (ERight c)
  assoc (EBoth (ELeft a) c) = EBoth a (ERight c)
  assoc (EBoth (ERight b) c) = ERight (EBoth b c)
  assoc (EBoth (EBoth a b) c) = EBoth a (EBoth b c)

  unassoc ENone = ENone
  unassoc (ELeft a) = ELeft (ELeft a)
  unassoc (ERight ENone) = ELeft ENone
  unassoc (ERight (ELeft b)) = ELeft (ERight b)
  unassoc (ERight (ERight c)) = ERight c
  unassoc (ERight (EBoth b c)) = EBoth (ERight b) c
  unassoc (EBoth a ENone) = ELeft (ELeft a)
  unassoc (EBoth a (ELeft b)) = ELeft (EBoth a b)
  unassoc (EBoth a (ERight c)) = EBoth (ELeft a) c
  unassoc (EBoth a (EBoth b c)) = EBoth (EBoth a b) c

instance SwapC Elr where
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

-- | similar to 'elr' without a separate EBoth combinator
--
-- >>> mergeElrWith [] (:[]) (pure . read) (++) (ELeft 123)
-- [123]
--
-- >>> mergeElrWith [] (:[]) (pure . read) (++) (EBoth 123 "11")
-- [123,11]
--
-- >>> mergeElrWith [999] (:[]) (pure . read) (++) ENone
-- [999]
--
mergeElrWith :: c -> (a -> c) -> (b -> c) -> (c -> c -> c) -> Elr a b -> c
mergeElrWith c fa fb fcc =
  \case
    ENone -> c
    ELeft a -> fa a
    ERight b -> fb b
    EBoth a b -> fcc (fa a) (fb b)

-- | destruct 'Elr'
--
-- >>> elr Nothing (Just . This) (Just . That) ((Just .) . These) (ELeft 10)
-- Just (This 10)
--
-- >>> elr Nothing (Just . This) (Just . That) ((Just .) . These) (EBoth 'x' 99)
-- Just (These 'x' 99)
--
-- >>> elr Nothing (Just . This) (Just . That) ((Just .) . These) ENone
-- Nothing
--
elr :: c -> (a -> c) -> (b -> c) -> (a -> b -> c) -> Elr a b -> c
elr c fa fb fab =
  \case
    ENone -> c
    ELeft a -> fa a
    ERight b -> fb b
    EBoth a b -> fab a b