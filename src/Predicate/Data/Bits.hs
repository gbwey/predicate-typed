{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | promoted bit manipulation functions
module Predicate.Data.Bits (
    type (.&.)
  , type (.|.)
  , type (.^.)
  , BitShift
  , BitShiftL
  , BitShiftR
  , BitRotate
  , BitRotateL
  , BitRotateR
  , BitSet
  , BitComplement
  , BitClear

  , PopCount
  , TestBit
  , Bit
  , ZeroBits
 ) where
import Predicate.Core
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import qualified Data.Bits as Bits
import Data.Bits (Bits(..))
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import Predicate.Prelude

-- | bitwise @and@ similar to 'Data.Bits..&.'
--
-- >>> pz @(344 .&. 123) ()
-- Val 88
--
data p .&. q deriving Show
infixl 7 .&.

instance ( P p a
         , P q a
         , Show (PP p a)
         , PP p a ~ PP q a
         , Bits (PP p a)
         ) => P (p .&. q) a where
  type PP (p .&. q) a = PP p a
  eval _ opts a = do
    let msg0 = "(.&.)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            d = p Bits..&. q
        in mkNode opts (Val d) (showL opts p <> " .&. " <> showL opts q <> " = " <> showL opts d) hhs


-- | bitwise @or@ similar to 'Data.Bits..|.'
--
-- >>> pz @(344 .|. 123) ()
-- Val 379
--
-- >>> pz @(Fst .|. Snd) (124,33)
-- Val 125
--
data p .|. q deriving Show
infixl 5 .|.

instance ( P p a
         , P q a
         , Show (PP p a)
         , PP p a ~ PP q a
         , Bits (PP p a)
         ) => P (p .|. q) a where
  type PP (p .|. q) a = PP p a
  eval _ opts a = do
    let msg0 = "(.|.)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            d = p Bits..|. q
        in mkNode opts (Val d) (showL opts p <> " .|. " <> showL opts q <> " = " <> showL opts d) hhs

-- | bitwise @xor@ similar to 'Data.Bits.xor'
--
-- >>> pz @(344 .^. 123) ()
-- Val 291
--
data p .^. q deriving Show
infixl 5 .^.

instance ( P p a
         , P q a
         , Show (PP p a)
         , PP p a ~ PP q a
         , Bits (PP p a)
         ) => P (p .^. q) a where
  type PP (p .^. q) a = PP p a
  eval _ opts a = do
    let msg0 = "(.^.)"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            d = p `Bits.xor` q
        in mkNode opts (Val d) (showL opts p <> " .^. " <> showL opts q <> " = " <> showL opts d) hhs

data BitFunction =
     BFShift
   | BFShiftL
   | BFShiftR
   | BFRotate
   | BFRotateL
   | BFRotateR
   | BFSet
   | BFClear
   | BFComplement
   deriving (Show,Eq)

class BitFunctionC (fn :: BitFunction) where
  bitFunction :: Bits a => (String, a -> Int -> a)
instance BitFunctionC 'BFShift where
  bitFunction = ("shift", Bits.shift)
instance BitFunctionC 'BFShiftL where
  bitFunction = ("shiftL", Bits.shiftL)
instance BitFunctionC 'BFShiftR where
  bitFunction = ("shiftR", Bits.shiftR)
instance BitFunctionC 'BFRotate where
  bitFunction = ("rotate", Bits.rotate)
instance BitFunctionC 'BFRotateL where
  bitFunction = ("rotateL", Bits.rotateL)
instance BitFunctionC 'BFRotateR where
  bitFunction = ("rotateR", Bits.rotateR)
instance BitFunctionC 'BFSet where
  bitFunction = ("setBit", Bits.setBit)
instance BitFunctionC 'BFClear where
  bitFunction = ("clearBit", Bits.clearBit)
instance BitFunctionC 'BFComplement where
  bitFunction = ("complementBit", Bits.complementBit)

-- flips the function where p is the integral
data BitImpl (fn :: BitFunction) p q deriving Show

instance ( P p a
         , P q a
         , Show (PP q a)
         , Bits (PP q a)
         , Integral (PP p a)
         , BitFunctionC fn
         ) => P (BitImpl fn p q) a where
  type PP (BitImpl fn p q) a = PP q a
  eval _ opts a = do
    let msg0 = ss
        (ss,fn) = bitFunction @fn
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            d = fn q p
        in mkNode opts (Val d) (ss <> " " <> showL opts p <> " " <> showL opts q <> " = " <> showL opts d) hhs

-- | shift by @p@ using @q@: similar to flipped version of 'Data.Bits.shift'
--
-- >>> pz @(BitShift 1 7) ()
-- Val 14
--
-- >>> pz @(BitShift 1 Id) 123
-- Val 246
--
data BitShift p q deriving Show
type BitShiftT p q = BitImpl 'BFShift p q

instance P (BitShiftT p q) x => P (BitShift p q) x where
  type PP (BitShift p q) x = PP (BitShiftT p q) x
  eval _ = eval (Proxy @(BitShiftT p q))

-- | shift left by @p@ using @q@: similar to flipped version of 'Data.Bits.shiftL'
--
-- >>> pz @(BitShiftL 1 Id) 123
-- Val 246
--
data BitShiftL p q deriving Show
type BitShiftLT p q = BitImpl 'BFShiftL p q

instance P (BitShiftLT p q) x => P (BitShiftL p q) x where
  type PP (BitShiftL p q) x = PP (BitShiftLT p q) x
  eval _ = eval (Proxy @(BitShiftLT p q))

-- | shift right by @p@ using @q@: similar to flipped version of 'Data.Bits.shiftR'
--
-- >>> pz @(BitShiftR 1 Id) 123
-- Val 61
--
data BitShiftR p q deriving Show
type BitShiftRT p q = BitImpl 'BFShiftR p q

instance P (BitShiftRT p q) x => P (BitShiftR p q) x where
  type PP (BitShiftR p q) x = PP (BitShiftRT p q) x
  eval _ = eval (Proxy @(BitShiftRT p q))

-- | rotate by @p@ using @q@: similar to flipped version of 'Data.Bits.rotate'
--
-- >>> pz @(BitRotate 2 Id) 7
-- Val 28
--
data BitRotate p q deriving Show
type BitRotateT p q = BitImpl 'BFRotate p q

instance P (BitRotateT p q) x => P (BitRotate p q) x where
  type PP (BitRotate p q) x = PP (BitRotateT p q) x
  eval _ = eval (Proxy @(BitRotateT p q))

-- | rotate left by @p@ using @q@: similar to flipped version of 'Data.Bits.rotateL'
--
-- >>> pz @(BitRotateL 2 Id) 7
-- Val 28
--
data BitRotateL p q deriving Show
type BitRotateLT p q = BitImpl 'BFRotateL p q

instance P (BitRotateLT p q) x => P (BitRotateL p q) x where
  type PP (BitRotateL p q) x = PP (BitRotateLT p q) x
  eval _ = eval (Proxy @(BitRotateLT p q))

-- | rotate right by @p@ using @q@: similar to flipped version of 'Data.Bits.rotateR'
--
-- >>> pz @(BitRotateR 2 Id) 7
-- Val 1
--

data BitRotateR p q deriving Show
type BitRotateRT p q = BitImpl 'BFRotateR p q

instance P (BitRotateRT p q) x => P (BitRotateR p q) x where
  type PP (BitRotateR p q) x = PP (BitRotateRT p q) x
  eval _ = eval (Proxy @(BitRotateRT p q))

-- | set the bit at @p@ using @q@: similar to flipped version of 'Data.Bits.setBit'
--
-- >>> pz @(BitSet 0 Id) 8
-- Val 9
--
data BitSet p q deriving Show
type BitSetT p q = BitImpl 'BFSet p q

instance P (BitSetT p q) x => P (BitSet p q) x where
  type PP (BitSet p q) x = PP (BitSetT p q) x
  eval _ = eval (Proxy @(BitSetT p q))

-- | clear the bit at @p@ using @q@: similar to flipped version of 'Data.Bits.clearBit'
--
-- >>> pz @(BitClear 2 Id) 7
-- Val 3
--
data BitClear p q deriving Show
type BitClearT p q = BitImpl 'BFClear p q

instance P (BitClearT p q) x => P (BitClear p q) x where
  type PP (BitClear p q) x = PP (BitClearT p q) x
  eval _ = eval (Proxy @(BitClearT p q))

-- | complement the bit at @p@ using @q@: similar to flipped version of 'Data.Bits.complementBit'
--
-- >>> pz @(BitComplement 1 Id) 7
-- Val 5
--
data BitComplement p q deriving Show
type BitComplementT p q = BitImpl 'BFComplement p q

instance P (BitComplementT p q) x => P (BitComplement p q) x where
  type PP (BitComplement p q) x = PP (BitComplementT p q) x
  eval _ = eval (Proxy @(BitComplementT p q))

-- | test the bit at @p@ using @q@: similar to flipped version of 'Data.Bits.testBit'
--
-- >>> pz @(TestBit 2 Id) 7
-- Val True
--
-- >>> pz @(TestBit 2 Id) 8
-- Val False
--
data TestBit p q deriving Show

instance ( P p a
         , P q a
         , Show (PP q a)
         , Bits (PP q a)
         , Integral (PP p a)
         ) => P (TestBit p q) a where
  type PP (TestBit p q) a = Bool
  eval _ opts a = do
    let msg0 = "TestBit"
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            d = Bits.testBit q p
        in mkNodeB opts d (msg0 <> " " <> showL opts p <> " " <> showL opts q <> " = " <> showL opts d) hhs

-- | count number of bits at @p@: similar to 'Data.Bits.popCount'
--
-- >>> pz @(PopCount Id) 7
-- Val 3
--
-- >>> pz @(PopCount Id) 8
-- Val 1
--
-- >>> pz @(PopCount Id) (-7)
-- Val (-3)
--
data PopCount p deriving Show

instance ( P p a
         , Show (PP p a)
         , Bits (PP p a)
         ) => P (PopCount p) a where
  type PP (PopCount p) a = Int
  eval _ opts a = do
    let msg0 = "PopCount"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Bits.popCount p
        in mkNode opts (Val d) (msg0 <> " " <> showL opts p <> " = " <> showL opts d) [hh pp]

-- | create a 'Data.Bits.Bits' for type @t with the bit at @p@ and all the others set to zero: similar to 'Data.Bits.bit'
--
-- >>> pz @(Bit Int Id) 0
-- Val 1
--
-- >>> pz @(Bit Int Id) 3
-- Val 8
--
data Bit t p deriving Show

instance ( P p a
         , Show t
         , Bits t
         , Integral (PP p a)
         ) => P (Bit t p) a where
  type PP (Bit t p) a = t
  eval _ opts a = do
    let msg0 = "Bit"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right (fromIntegral -> p) ->
        let d = Bits.bit @t p
        in mkNode opts (Val d) (msg0 <> " " <> showL opts p <> " = " <> showL opts d) [hh pp]


-- | create a 'Data.Bits.Bits' for type @t with all bits set to zero: similar to 'Data.Bits.zeroBits'
--
-- >>> pz @(ZeroBits Int) ()
-- Val 0
--
data ZeroBits t deriving Show

instance ( Show t
         , Bits t
         ) => P (ZeroBits t) a where
  type PP (ZeroBits t) a = t
  eval _ opts _ = pure $
    let msg0 = "ZeroBits"
        d = Bits.zeroBits @t
    in mkNode opts (Val d) (msg0 <> " " <> " = " <> showL opts d) []
