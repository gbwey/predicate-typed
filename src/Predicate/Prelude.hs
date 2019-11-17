-- no types referring to types! else lose use of _ as Type
{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoOverloadedLists #-}
{- |
     Dsl for evaluating and displaying type level expressions

     Contains instances of the class 'P' for evaluating expressions at the type level.
-}
module Predicate.Prelude (
  -- ** boolean expressions
    type (&&)
  , type (||)
  , type (~>)
  , Not
  , Ands
  , Ors
  , Asc
  , Asc'
  , Desc
  , Desc'
  , Between
  , type (<..>)
  , Between'
  , All
  , Any
  , AllPositive
  , Positive
  , AllNegative
  , Negative

  -- ** regex expressions
  , Re
  , Re'
  , Rescan
  , Rescan'
  , RescanRanges
  , RescanRanges'
  , Resplit
  , Resplit'
  , ReplaceAll
  , ReplaceAll'
  , ReplaceOne
  , ReplaceOne'
  , ReplaceAllString
  , ReplaceAllString'
  , ReplaceOneString
  , ReplaceOneString'
  , ReplaceFn
  , ReplaceFn1
  , ReplaceFn2
  , ReplaceFn3

  -- ** tuple expressions
  , Fst
  , Snd
  , Thd
  , L1
  , L2
  , L3
  , L4
  , L5
  , L6
  , Dup
  , Swap
  , SwapC(..)
  , Assoc
  , Unassoc
  , Pairs

 -- ** character expressions
  , IsLower
  , IsUpper
  , IsNumber
  , IsSpace
  , IsPunctuation
  , IsControl
  , IsHexDigit
  , IsOctDigit
  , IsSeparator
  , IsLatin1

  -- ** datetime expressions
  , FormatTimeP
  , ParseTimeP
  , ParseTimeP'
  , ParseTimes
  , ParseTimes'
  , MkDay
  , MkDay'
  , UnMkDay

  -- ** numeric expressions
  , type (+)
  , type (-)
  , type (*)
  , type (/)
  , Negate
  , Abs
  , Signum
  , FromInteger
  , FromInteger'
  , FromIntegral
  , FromIntegral'
  , Truncate
  , Truncate'
  , Ceiling
  , Ceiling'
  , Floor
  , Floor'
  , Even
  , Odd
  , Div
  , Mod
  , DivMod
  , QuotRem
  , Quot
  , Rem

  -- *** rational numbers
  , type (%)
  , type (-%)
  , ToRational
  , FromRational
  , FromRational'

 -- ** proxy expressions
  , MkProxy
  , ProxyT
  , ProxyT'
  , Unproxy

 -- ** read / show expressions
  , ShowP
  , ReadP
  , ReadP'
  , ReadQ
  , ReadQ'
  , ReadMaybe
  , ReadMaybe'
  , ReadBase
  , ReadBase'
  , ReadBaseInt
  , ShowBase

  -- ** aeson expressions
  , ParseJson'
  , ParseJson
  , EncodeJson
  , ParseJsonFile'
  , ParseJsonFile

  -- ** arrow expressions
  , type (&&&)
  , type (***)
  , First
  , Second
  , type (|||)
  , type (+++)

 -- ** compare expressions
  , type (>)
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
  , OrdP
  , type (==!)
  , OrdA'
  , OrdA
  , OrdI
  , type (===~)
  , Cmp
  , CmpI

  -- ** enum expressions
  , Succ
  , Pred
  , FromEnum
  , ToEnum
  , ToEnum'
  , EnumFromTo
  -- *** bounded enum expressions
  , SuccB
  , SuccB'
  , PredB
  , PredB'
  , ToEnumBDef
  , ToEnumBDef'
  , ToEnumBFail

 -- ** wrap / unwrap expressions
  , Unwrap
  , Wrap
  , Wrap'
  , Coerce
  , Coerce2

  -- ** list / foldable expressions
  , Map
  , Concat
  , ConcatMap
  , Partition
  , Filter
  , Break
  , Span
  , Intercalate
  , Elem
  , Inits
  , Tails
  , Ones
  , OneP
  , Len
  , Length
  , PadL
  , PadR
  , Cycle
  , SplitAts
  , SplitAt
  , Take
  , Drop
  , Min
  , Max
  , Sum
  , IsEmpty
  , Null
  , Null'
  , ToList
  , ToList'
  , IToList
  , IToList'
  , FromList
  , EmptyList
  , EmptyList'
  , Singleton
  , Reverse
  , ReverseL
  , SortBy
  , SortOn
  , SortOnDesc
  , Remove
  , Keep
 -- *** overloaded list expressions
  , ToListExt
  , FromListExt

 -- ** maybe expressions
  , MkNothing
  , MkNothing'
  , MkJust
  , IsNothing
  , IsJust
  , MapMaybe
  , CatMaybes
  , Just
  , JustDef
  , JustFail
  , MaybeIn
  , MaybeBool

 -- ** either expressions
  , PartitionEithers
  , IsLeft
  , IsRight
  , MkLeft
  , MkLeft'
  , MkRight
  , MkRight'
  , Left'
  , Right'
  , LeftDef
  , LeftFail
  , RightDef
  , RightFail
  , EitherBool
  , EitherIn

  -- ** semigroup / monoid expressions
  , type (<>)
  , MConcat
  , STimes
  , Sapa
  , Sapa'
  , MEmptyT
  , MEmptyT'
  , MEmptyP
  , MEmpty2
  , MEmpty2'

  -- ** indexing expressions
  , Ix
  , Ix'
  , IxL
  , type (!!)
  , Lookup
  , LookupDef
  , LookupDef'
  , LookupFail
  , LookupFail'

 -- cons / uncons expressions
  , type (:+)
  , type (+:)
  , Uncons
  , Unsnoc
  , Head
  , Tail
  , Init
  , Last
  , HeadDef
  , HeadFail
  , TailDef
  , TailFail
  , LastDef
  , LastFail
  , InitDef
  , InitFail

 -- ** these expressions
  , PartitionThese
  , Thiss
  , Thats
  , Theses
  , This'
  , That'
  , These'
  , IsThis
  , IsThat
  , IsThese
  , MkThis
  , MkThis'
  , MkThat
  , MkThat'
  , MkThese
  , ThisDef
  , ThisFail
  , ThatDef
  , ThatFail
  , TheseDef
  , TheseFail
  , TheseIn
  , TheseId
  , TheseX

 -- ** fold / unfold expressions
  , Scanl
  , ScanN
  , ScanNA
  , FoldN
  , FoldL
  , Unfoldr
  , IterateN
  , IterateUntil
  , IterateWhile
  , IterateNWhile
  , IterateNUntil

  -- ** failure expressions
  , Fail
  , Failp
  , Failt
  , FailS
  , Catch
  , Catch'

  -- ** zip expressions
  , ZipThese
  , ZipL
  , ZipR
  , Zip
  , Unzip
  , Unzip3

  -- ** conditional expressions
  , If
  , Case
  , Case'
  , Case''
  , Guards
  , GuardsQuick
  , Guard
  , ExitWhen
  , GuardSimple
  , GuardsN
  , GuardsDetail

  , Bools
  , BoolsQuick
  , BoolsN

  -- ** IO expressions
  , ReadFile
  , FileExists
  , ReadDir
  , DirExists
  , ReadEnv
  , ReadEnvAll
  , TimeUtc
  , TimeZt
  , AppendFile
  , WriteFile
  , WriteFile'
  , Stdout
  , Stderr
  , Stdin

  -- ** string expressions
  , ToLower
  , ToUpper
  , TrimBoth
  , TrimL
  , TrimR
  , StripR
  , StripL
  , IsPrefix
  , IsInfix
  , IsSuffix
  , IsPrefixI
  , IsInfixI
  , IsSuffixI
  , FromStringP
  , FromStringP'

  -- ** print expressions
  , PrintF
  , PrintL
  , PrintT

  -- ** higher order expressions
  , Pure
  , Pure2
  , FoldMap
  , type (<$)
  , type (<*)
  , type (*>)
  , FMapFst
  , FMapSnd
  , Sequence
  , Traverse
  , Join
  , EmptyT
  , type (<|>)
  , Extract
  , Duplicate

  -- ** expression combinators
  , type ($)
  , type (&)
  , Do
  , Dot
  , RDot
  , type (>>)
  , type (<<)
  , type (>>>)
  , DoN
  , type ($$)
  , type ($&)
  , K
  , Hide
  , Hole
  , Skip
  , type (|>)
  , type (>|)
  , type (>|>)

  -- *** parallel expressions
  , Para
  , ParaN
  , Repeat

  -- ** miscellaneous
  , Both
  , Prime
  , Luhn
  , Char1
 ) where
import Predicate.Core
import Predicate.Util
import Safe (succMay, predMay, toEnumMay)
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Control.Lens hiding (iall)
--import Control.Lens (Unwrapped, Wrapped, _Unwrapped', _Wrapped', Ixed, IxValue, Index, Reversing, Cons, Snoc, AsEmpty, FoldableWithIndex, allOf, (%~), (<&>), (^.), (^?), coerced, view, reversed, ix, cons, snoc, _Cons, _Snoc, (^?!), (.~), itoList, Identity(..), _Empty, has)
import Data.List
import qualified Data.Text.Lens as TL
import Data.Proxy
import Control.Applicative
import Data.Typeable
import Control.Monad.Except
import qualified Control.Exception as E
import Data.Kind (Type)
import qualified Text.Regex.PCRE.Heavy as RH
import Data.String
import Data.Foldable
import Data.Maybe
import Control.Arrow
import qualified Data.Semigroup as SG
import Data.Semigroup (Semigroup(..))
import Numeric
import Data.Char
import Data.Function
import Data.These (These(..))
import Data.Ratio
import Data.Time
import Data.Coerce
import Data.Void
import qualified Data.Sequence as Seq
import Text.Printf
import System.Directory
import Control.Comonad
import System.IO
import System.Environment
import qualified GHC.Exts as GE
import Data.Bool
import Data.Either
import qualified Data.Type.Equality as DE
import Data.Time.Calendar.WeekDate
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Text as T
-- >>> import Safe (readNote)

-- | a type level predicate for a monotonic increasing list
--
-- >>> pl @Asc "aaacdef"
-- True (All(6))
-- TrueT
--
-- >>> pz @Asc [1,2,3,4,5,5,7]
-- True
-- TrueT
--
-- >>> pz @Asc' [1,2,3,4,5,5,7]
-- False
-- FalseT
--
-- >>> pz @Asc "axacdef"
-- False
-- FalseT
--


-- | a type level predicate for a monotonic increasing list
data Asc
type AscT = All (Fst Id <= Snd Id) Pairs

instance P AscT x => P Asc x where
  type PP Asc x = PP AscT x
  eval _ = eval (Proxy @AscT)

-- | a type level predicate for a strictly increasing list
data Asc'
type AscT' = All (Fst Id < Snd Id) Pairs

instance P AscT' x => P Asc' x where
  type PP Asc' x = PP AscT' x
  eval _ = eval (Proxy @AscT')

-- | a type level predicate for a monotonic decreasing list
data Desc
type DescT = All (Fst Id >= Snd Id) Pairs

instance P DescT x => P Desc x where
  type PP Desc x = PP DescT x
  eval _ = eval (Proxy @DescT)
-- | a type level predicate for a strictly decreasing list
data Desc'
type DescT' = All (Fst Id > Snd Id) Pairs

instance P DescT' x => P Desc' x where
  type PP Desc' x = PP DescT' x
  eval _ = eval (Proxy @DescT')


--type AscAlt = SortOn Id Id == Id
--type DescAlt = SortOnDesc Id Id == Id

-- | A predicate that determines if the value is between \'p\' and \'q\'
--
-- >>> pz @(Between' 5 8 Len) [1,2,3,4,5,5,7]
-- True
-- TrueT
--
-- >>> pz @(Between 5 8) 6
-- True
-- TrueT
--
-- >>> pl @(Between 5 8) 9
-- False (9 <= 8)
-- FalseT
--
-- >>> pz @(10 % 4 <..> 40 % 5) 4
-- True
-- TrueT
--
-- >>> pz @(10 % 4 <..> 40 % 5) 33
-- False
-- FalseT
--
data Between' p q r -- reify as it is used a lot! nicer specific messages at the top level!

instance (Ord (PP p x)
       , Show (PP p x)
       , PP r x ~ PP p x
       , PP r x ~ PP q x
       , P p x
       , P q x
       , P r x
       ) => P (Between' p q r) x where
  type PP (Between' p q r) x = Bool
  eval _ opts x = do
    let msg0 = "Between"
    rr <- eval (Proxy @r) opts x
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right r -> do
        lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x [hh rr]
        pure $ case lr of
          Left e -> e
          Right (p,q,pp,qq) ->
            let hhs = [hh rr, hh pp, hh qq]
            in if p <= r && r <= q then mkNodeB opts True [show p <> " <= " <> show r <> " <= " <> show q] hhs
               else if p > r then mkNodeB opts False [show p <> " <= " <> show r] hhs
               else mkNodeB opts False [show r <> " <= " <> show q] hhs

data Between p q
type BetweenT p q = Between' p q Id

instance P (BetweenT p q) x => P (Between p q) x where
  type PP (Between p q) x = PP (BetweenT p q) x
  eval _ = eval (Proxy @(BetweenT p q))

data p <..> q
infix 4 <..>

type BetweenTT p q = Between p q

instance P (BetweenTT p q) x => P (p <..> q) x where
  type PP (p <..> q) x = PP (BetweenTT p q) x
  eval _ = eval (Proxy @(BetweenTT p q))

-- | similar to 'all'
--
-- >>> pl @(All (Between 1 8) Id) [7,3,4,1,2,9,0,1]
-- False (All(8) i=5 (9 <= 8))
-- FalseT
--
-- >>> pz @(All Odd Id) [1,5,11,5,3]
-- True
-- TrueT
--
-- >>> pz @(All Odd Id) []
-- True
-- TrueT
--
-- >>> pe @(All Even Id) [1,5,11,5,3]
-- False All(5) i=0 (1 == 0)
-- |
-- +- P Id [1,5,11,5,3]
-- |
-- +- False i=0:1 == 0
-- |  |
-- |  +- P 1 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=1:1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=2:1 == 0
-- |  |
-- |  +- P 11 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- +- False i=3:1 == 0
-- |  |
-- |  +- P 5 `mod` 2 = 1
-- |  |  |
-- |  |  +- P I
-- |  |  |
-- |  |  `- P '2
-- |  |
-- |  `- P '0
-- |
-- `- False i=4:1 == 0
--    |
--    +- P 3 `mod` 2 = 1
--    |  |
--    |  +- P I
--    |  |
--    |  `- P '2
--    |
--    `- P '0
-- FalseT
--
data All p q

instance (P p a
        , PP p a ~ Bool
        , PP q x ~ f a
        , P q x
        , Show a
        , Foldable f
        ) => P (All p q) x where
  type PP (All p q) x = Bool
  eval _ opts x = do
    let msg0 = "All"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalBool (Proxy @p) opts a) [0::Int ..] (toList q)
            pure $ case splitAndAlign opts [msg0] ts of
                 Left e -> e
                 Right abcs ->
                   let hhs = hh qq : map (hh . fixit) ts
                       msg1 = msg0 ++ "(" ++ show (length q) ++ ")"
                   in case find (not . view _1) abcs of
                        Nothing -> mkNodeB opts True [msg1] hhs
                        Just (_,(i,_),tt) ->
                          mkNodeB opts False [msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt] hhs

chkSize :: Foldable t => POpts -> String -> t a -> [Holder] -> Either (TT x) ()
chkSize opts msg0 xs hhs =
  case splitAt _MX (toList xs) of
    (_,[]) -> Right ()
    (_,_:_) -> Left $ mkNode opts (FailT (msg0 <> " list size exceeded")) [msg0 <> " list size exceeded: max is " ++ show _MX] hhs

showIndex :: (Show i, Num i) => i -> String
showIndex i = show (i+0)
-- | similar to 'any'
--
-- >>> pl @(Any Even Id) [1,5,11,5,3]
-- False (Any(5))
-- FalseT
--
-- >>> pl @(Any Even Id) [1,5,112,5,3]
-- True (Any(5) i=2 (0 == 0))
-- TrueT
--
-- >>> pz @(Any Even Id) []
-- False
-- FalseT
--
data Any p q

instance (P p a
        , PP p a ~ Bool
        , PP q x ~ f a
        , P q x
        , Show a
        , Foldable f
        ) => P (Any p q) x where
  type PP (Any p q) x = Bool
  eval _ opts x = do
    let msg0 = "Any"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
            ts <- zipWithM (\i a -> ((i, a),) <$> evalBool (Proxy @p) opts a) [0::Int ..] (toList q)
            pure $ case splitAndAlign opts [msg0] ts of
                 Left e -> e
                 Right abcs ->
                   let hhs = hh qq : map (hh . fixit) ts
                       msg1 = msg0 ++ "(" ++ show (length q) ++ ")"
                   in case find (view _1) abcs of
                        Nothing -> mkNodeB opts False [msg1] hhs
                        Just (_,(i,_),tt) ->
                          mkNodeB opts True [msg1 <> " i=" ++ showIndex i ++ " " <> topMessage tt] hhs


-- | a type level predicate for all positive elements in a list
--
-- >>> pz @AllPositive [1,5,10,2,3]
-- True
-- TrueT
--
-- >>> pz @AllPositive [0,1,5,10,2,3]
-- False
-- FalseT
--
-- >>> pz @AllPositive [3,1,-5,10,2,3]
-- False
-- FalseT
--
-- >>> pz @AllNegative [-1,-5,-10,-2,-3]
-- True
-- TrueT
--
data AllPositive
type AllPositiveT = All Positive Id

instance P AllPositiveT x => P AllPositive x where
  type PP AllPositive x = PP AllPositiveT x
  eval _ = eval (Proxy @AllPositiveT)

-- | a type level predicate for all negative elements in a list
data AllNegative
type AllNegativeT = All Negative Id

instance P AllNegativeT x => P AllNegative x where
  type PP AllNegative x = PP AllNegativeT x
  eval _ = eval (Proxy @AllNegativeT)


type Positive = Gt 0

type Negative = Lt 0

-- | 'unzip' equivalent
--
-- >>> pz @Unzip (zip [1..5] "abcd")
-- Present ([1,2,3,4],"abcd")
-- PresentT ([1,2,3,4],"abcd")
--
data Unzip
type UnzipT = '(Map (Fst Id) Id, Map (Snd Id) Id)

instance P UnzipT x => P Unzip x where
  type PP Unzip x = PP UnzipT x
  eval _ = eval (Proxy @UnzipT)


-- | 'unzip3' equivalent
--
-- >>> pz @Unzip3 (zip3 [1..5] "abcd" (cycle [True,False]))
-- Present ([1,2,3,4],"abcd",[True,False,True,False])
-- PresentT ([1,2,3,4],"abcd",[True,False,True,False])
--
data Unzip3
type Unzip3T = '(Map (Fst Id) Id, Map (Snd Id) Id, Map (Thd Id) Id)

instance P Unzip3T x => P Unzip3 x where
  type PP Unzip3 x = PP Unzip3T x
  eval _ = eval (Proxy @Unzip3T)


-- | represents a predicate using a 'Symbol' as a regular expression
-- evaluates 'Re' and returns True if there is a match
--
-- >>> pz @(Re "^\\d{2}:\\d{2}:\\d{2}$" Id) "13:05:25"
-- True
-- TrueT
--
data Re' (rs :: [ROpt]) p q
data Re p q
type ReT p q = Re' '[] p q

instance P (ReT p q) x => P (Re p q) x where
  type PP (Re p q) x = PP (ReT p q) x
  eval _ = eval (Proxy @(ReT p q))

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Re' rs p q) x where
  type PP (Re' rs p q) x = Bool
  eval _ opts x = do
    let msg0 = "Re" <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
            Left tta -> tta
            Right regex ->
               let b = q RH.=~ regex
               in mkNodeB opts b [msg1 <> showLit1 opts " | " q] hhs

-- only way with rescan is to be explicit: no repeats! and useanchors but not (?m)
-- or just use Re' but then we only get a bool ie doesnt capture groups
-- rescan returns Right [] as an failure!
-- [] is failure!


-- | runs a regex matcher returning the original values and optionally any groups
--
-- >>> pz @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
-- Present [("13:05:25",["13","05","25"])]
-- PresentT [("13:05:25",["13","05","25"])]
--
-- >>> pz @(Rescan (Snd Id) "13:05:25") ('a',"^(\\d{2}):(\\d{2}):(\\d{2})$")
-- Present [("13:05:25",["13","05","25"])]
-- PresentT [("13:05:25",["13","05","25"])]
--
data Rescan' (rs :: [ROpt]) p q
data Rescan p q
type RescanT p q = Rescan' '[] p q

instance P (RescanT p q) x => P (Rescan p q) x where
  type PP (Rescan p q) x = PP (RescanT p q) x
  eval _ = eval (Proxy @(RescanT p q))


instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Rescan' rs p q) x where
  type PP (Rescan' rs p q) x = [(String, [String])]
  eval _ opts x = do
    let msg0 = "Rescan" <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.scan regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> show1 opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> show1 opts " | " q] [hh pp, hh qq]
              (b, _) -> mkNode opts (PresentT b) [lit01 opts msg1 b q] [hh pp, hh qq]


-- | similar to 'Rescan' but gives the column start and ending positions instead of values
--
-- >>> pz @(RescanRanges "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
-- Present [((0,8),[(0,2),(3,5),(6,8)])]
-- PresentT [((0,8),[(0,2),(3,5),(6,8)])]
--
data RescanRanges' (rs :: [ROpt]) p q

data RescanRanges p q
type RescanRangesT p q = RescanRanges' '[] p q

instance P (RescanRangesT p q) x => P (RescanRanges p q) x where
  type PP (RescanRanges p q) x = PP (RescanRangesT p q) x
  eval _ = eval (Proxy @(RescanRangesT p q))


instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (RescanRanges' rs p q) x where
  type PP (RescanRanges' rs p q) x = [((Int,Int), [(Int,Int)])]
  eval _ opts x = do
    let msg0 = "RescanRanges" <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.scanRanges regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> show1 opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> show1 opts " | " q] hhs
              (b, _) -> mkNode opts (PresentT b) [lit01 opts msg1 b q] hhs

-- | splits a string on a regex delimiter
--
-- >>> pz @(Resplit "\\." Id) "141.201.1.22"
-- Present ["141","201","1","22"]
-- PresentT ["141","201","1","22"]
--
-- >>> pz @(Resplit (Singleton (Fst Id)) (Snd Id)) (':', "12:13:1")
-- Present ["12","13","1"]
-- PresentT ["12","13","1"]
--
-- >>> pl @(Resplit' '[ 'Caseless ] "aBc" Id) "123AbC456abc"
-- Present ["123","456",""] (Resplit (aBc) ["123","456",""] | 123AbC456abc)
-- PresentT ["123","456",""]
--
data Resplit' (rs :: [ROpt]) p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Resplit' rs p q) x where
  type PP (Resplit' rs p q) x = [String]
  eval _ opts x = do
    let msg0 = "Resplit" <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.split regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> show1 opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> show1 opts " | " q] hhs
              (b, _) -> mkNode opts (PresentT b) [lit01 opts msg1 b q] hhs

data Resplit p q
type ResplitT p q = Resplit' '[] p q

instance P (ResplitT p q) x => P (Resplit p q) x where
  type PP (Resplit p q) x = PP (ResplitT p q) x
  eval _ = eval (Proxy @(ResplitT p q))

-- | limit the size of the lists
_MX :: Int
_MX = 100

-- | replaces regex \'s\' with a string \'s1\' inside the value
--
-- >>> pz @(ReplaceAllString "\\." ":" Id) "141.201.1.22"
-- Present "141:201:1:22"
-- PresentT "141:201:1:22"
--
data ReplaceImpl (alle :: Bool) (rs :: [ROpt]) p q r

instance (GetBool b
        , GetROpts rs
        , PP p x ~ String
        , PP q x ~ RReplace
        , PP r x ~ String
        , P p x
        , P q x
        , P r x
        ) => P (ReplaceImpl b rs p q r) x where
  type PP (ReplaceImpl b rs p q r) x = String
  eval _ opts x = do
    let msg0 = "Replace" <> (if alle then "All" else "One") <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
        alle = getBool @b
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> pure tta
          Right regex -> do
            rr <- eval (Proxy @r) opts x
            pure $ case getValueLR opts msg0 rr hhs of
              Left e -> e
              Right r ->
               let ret :: String
                   ret = case q of
                           RReplace s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace1 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace2 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace3 s -> (if alle then RH.gsub else RH.sub) regex s r
               in mkNode opts (PresentT ret) [msg1 <> showLit0 opts " " r <> showLit1 opts " | " ret] (hhs <> [hh rr])

data ReplaceAll' (rs :: [ROpt]) p q r
type ReplaceAllT' (rs :: [ROpt]) p q r = ReplaceImpl 'True rs p q r

instance P (ReplaceAllT' rs p q r) x => P (ReplaceAll' rs p q r) x where
  type PP (ReplaceAll' rs p q r) x = PP (ReplaceAllT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceAllT' rs p q r))

data ReplaceAll p q r
type ReplaceAllT p q r = ReplaceAll' '[] p q r

instance P (ReplaceAllT p q r) x => P (ReplaceAll p q r) x where
  type PP (ReplaceAll p q r) x = PP (ReplaceAllT p q r) x
  eval _ = eval (Proxy @(ReplaceAllT p q r))

data ReplaceOne' (rs :: [ROpt]) p q r
type ReplaceOneT' (rs :: [ROpt]) p q r = ReplaceImpl 'False rs p q r

instance P (ReplaceOneT' rs p q r) x => P (ReplaceOne' rs p q r) x where
  type PP (ReplaceOne' rs p q r) x = PP (ReplaceOneT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceOneT' rs p q r))

-- | replace first occurrence of string \'p\' with '\q'\ in \'r\'
--
-- >>> pl @(ReplaceOneString "abc" "def" Id) "123abc456abc"
-- Present "123def456abc" (ReplaceOne' [] (abc) 123abc456abc | 123def456abc)
-- PresentT "123def456abc"
--
data ReplaceOne p q r
type ReplaceOneT p q r = ReplaceOne' '[] p q r

instance P (ReplaceOneT p q r) x => P (ReplaceOne p q r) x where
  type PP (ReplaceOne p q r) x = PP (ReplaceOneT p q r) x
  eval _ = eval (Proxy @(ReplaceOneT p q r))

-- | replace all occurrences of string \'p\' with '\q'\ in \'r\'
--
-- >>> pl @(ReplaceAllString "abc" "def" Id) "123abc456abc"
-- Present "123def456def" (ReplaceAll' [] (abc) 123abc456abc | 123def456def)
-- PresentT "123def456def"
--
-- >>> pl @(ReplaceAllString' '[] "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456def" (ReplaceAll' [] (abc) 123AbC456abc | 123AbC456def)
-- PresentT "123AbC456def"
--
-- >>> pl @(ReplaceAllString' '[ 'Caseless ] "abc" "def" Id) "123AbC456abc"
-- Present "123def456def" (ReplaceAll (abc) 123AbC456abc | 123def456def)
-- PresentT "123def456def"
--
data ReplaceAllString' (rs :: [ROpt]) p q r
type ReplaceAllStringT' (rs :: [ROpt]) p q r = ReplaceAll' rs p (ReplaceFn q) r

instance P (ReplaceAllStringT' rs p q r) x => P (ReplaceAllString' rs p q r) x where
  type PP (ReplaceAllString' rs p q r) x = PP (ReplaceAllStringT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT' rs p q r))

data ReplaceAllString p q r
type ReplaceAllStringT p q r = ReplaceAllString' '[] p q r

instance P (ReplaceAllStringT p q r) x => P (ReplaceAllString p q r) x where
  type PP (ReplaceAllString p q r) x = PP (ReplaceAllStringT p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT p q r))

data ReplaceOneString' (rs :: [ROpt]) p q r
type ReplaceOneStringT' (rs :: [ROpt]) p q r = ReplaceOne' rs p (ReplaceFn q) r

instance P (ReplaceOneStringT' rs p q r) x => P (ReplaceOneString' rs p q r) x where
  type PP (ReplaceOneString' rs p q r) x = PP (ReplaceOneStringT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT' rs p q r))

data ReplaceOneString p q r
type ReplaceOneStringT p q r = ReplaceOneString' '[] p q r

instance P (ReplaceOneStringT p q r) x => P (ReplaceOneString p q r) x where
  type PP (ReplaceOneString p q r) x = PP (ReplaceOneStringT p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT p q r))

-- | Simple replacement string: see 'ReplaceAllString' and 'ReplaceOneString'
--
data ReplaceFn p

instance (PP p x ~ String
        , P p x) => P (ReplaceFn p) x where
  type PP (ReplaceFn p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = RReplace p
        in mkNode opts (PresentT b) [msg0 <> show1 opts " | " p] [hh pp]

-- | A replacement function @(String -> [String] -> String)@ which returns the whole match and the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
data ReplaceFn1 p

instance (PP p x ~ (String -> [String] -> String)
        , P p x) => P (ReplaceFn1 p) x where
  type PP (ReplaceFn1 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn1 (String -> [String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace1 f)) [msg0] [hh pp]

-- | A replacement function @(String -> String)@ that yields the whole match
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(ReplaceAll "\\." (ReplaceFn2 (Fst Id)) (Snd Id)) (\x -> x <> ":" <> x, "141.201.1.22")
-- Present "141.:.201.:.1.:.22"
-- PresentT "141.:.201.:.1.:.22"
--
data ReplaceFn2 p

instance (PP p x ~ (String -> String)
        , P p x) => P (ReplaceFn2 p) x where
  type PP (ReplaceFn2 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn2 (String -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace2 f)) [msg0] [hh pp]

-- | A replacement function @([String] -> String)@ which yields the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(ReplaceAll "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" (ReplaceFn3 (Fst Id)) (Snd Id)) (\ys -> intercalate  " | " $ map (show . succ . readNote @Int "invalid int") ys, "141.201.1.22")
-- Present "142 | 202 | 2 | 23"
-- PresentT "142 | 202 | 2 | 23"
--
data ReplaceFn3 p

instance (PP p x ~ ([String] -> String)
        , P p x) => P (ReplaceFn3 p) x where
  type PP (ReplaceFn3 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn3 ([String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace3 f)) [msg0] [hh pp]

-- | a predicate for determining if a string 'Data.Text.IsText' belongs to the given character set
--
-- >>> pz @IsLower "abc"
-- True
-- TrueT
--
-- >>> pz @IsLower "abcX"
-- False
-- FalseT
--
-- >>> pz @IsLower (T.pack "abcX")
-- False
-- FalseT
--
-- >>> pz @IsHexDigit "01efA"
-- True
-- TrueT
--
-- >>> pz @IsHexDigit "01egfA"
-- False
-- FalseT
--
-- | predicate for determining if a string is all lowercase
--
-- >>> pz @IsLower "abcdef213"
-- False
-- FalseT
--
-- >>> pz @IsLower "abcdef"
-- True
-- TrueT
--
-- >>> pz @IsLower ""
-- True
-- TrueT
--
-- >>> pz @IsLower "abcdefG"
-- False
-- FalseT
--
instance (GetCharSet cs
        , Show a
        , TL.IsText a
        ) => P (IsCharSet cs) a where
  type PP (IsCharSet cs) a = Bool
  eval _ opts as =
    let b = allOf TL.text f as
        msg0 = "IsCharSet " ++ show cs
        (cs,f) = getCharSet @cs
    in pure $ mkNodeB opts b [msg0 <> show1 opts " | " as] []

data IsCharSet (cs :: CharSet)

data CharSet = CLower
             | CUpper
             | CNumber
             | CSpace
             | CPunctuation
             | CControl
             | CHexDigit
             | COctDigit
             | CSeparator
             | CLatin1
             deriving Show

class GetCharSet (cs :: CharSet) where
  getCharSet :: (CharSet, Char -> Bool)
instance GetCharSet 'CLower where
  getCharSet = (CLower, isLower)
instance GetCharSet 'CUpper where
  getCharSet = (CUpper, isUpper)
instance GetCharSet 'CNumber where
  getCharSet = (CNumber, isNumber)
instance GetCharSet 'CPunctuation where
  getCharSet = (CPunctuation, isPunctuation)
instance GetCharSet 'CControl where
  getCharSet = (CControl, isControl)
instance GetCharSet 'CHexDigit where
  getCharSet = (CHexDigit, isHexDigit)
instance GetCharSet 'COctDigit where
  getCharSet = (COctDigit, isOctDigit)
instance GetCharSet 'CSeparator where
  getCharSet = (CSeparator, isSeparator)
instance GetCharSet 'CLatin1 where
  getCharSet = (CLatin1, isLatin1)

data IsLower
type IsLowerT = IsCharSet 'CLower

instance P IsLowerT x => P IsLower x where
  type PP IsLower x = PP IsLowerT x
  eval _ = eval (Proxy @IsLowerT)

data IsUpper
type IsUpperT = IsCharSet 'CUpper

instance P IsUpperT x => P IsUpper x where
  type PP IsUpper x = PP IsUpperT x
  eval _ = eval (Proxy @IsUpperT)

-- | predicate for determining if the string is all digits
--
-- >>> pz @IsNumber "213G"
-- False
-- FalseT
--
-- >>> pz @IsNumber "929"
-- True
-- TrueT
--
data IsNumber
type IsNumberT = IsCharSet 'CNumber
instance P IsNumberT x => P IsNumber x where
  type PP IsNumber x = PP IsNumberT x
  eval _ = eval (Proxy @IsNumberT)

data IsSpace
type IsSpaceT = IsCharSet 'CSpace
instance P IsSpaceT x => P IsSpace x where
  type PP IsSpace x = PP IsSpaceT x
  eval _ = eval (Proxy @IsSpaceT)

data IsPunctuation
type IsPunctuationT = IsCharSet 'CPunctuation
instance P IsPunctuationT x => P IsPunctuation x where
  type PP IsPunctuation x = PP IsPunctuationT x
  eval _ = eval (Proxy @IsPunctuationT)

data IsControl
type IsControlT = IsCharSet 'CControl
instance P IsControlT x => P IsControl x where
  type PP IsControl x = PP IsControlT x
  eval _ = eval (Proxy @IsControlT)

data IsHexDigit
type IsHexDigitT = IsCharSet 'CHexDigit
instance P IsHexDigitT x => P IsHexDigit x where
  type PP IsHexDigit x = PP IsHexDigitT x
  eval _ = eval (Proxy @IsHexDigitT)

data IsOctDigit
type IsOctDigitT = IsCharSet 'COctDigit
instance P IsOctDigitT x => P IsOctDigit x where
  type PP IsOctDigit x = PP IsOctDigitT x
  eval _ = eval (Proxy @IsOctDigitT)

data IsSeparator
type IsSeparatorT = IsCharSet 'CSeparator
instance P IsSeparatorT x => P IsSeparator x where
  type PP IsSeparator x = PP IsSeparatorT x
  eval _ = eval (Proxy @IsSeparatorT)

data IsLatin1
type IsLatin1T = IsCharSet 'CLatin1
instance P IsLatin1T x => P IsLatin1 x where
  type PP IsLatin1 x = PP IsLatin1T x
  eval _ = eval (Proxy @IsLatin1T)


-- | converts a string 'Data.Text.Lens.IsText' value to lower case
--
-- >>> pz @ToLower "HeLlO wOrld!"
-- Present "hello world!"
-- PresentT "hello world!"
--
data ToLower

instance (Show a, TL.IsText a) => P ToLower a where
  type PP ToLower a = a
  eval _ opts as =
    let msg0 = "ToLower"
        xs = as & TL.text %~ toLower
    in pure $ mkNode opts (PresentT xs) [show01 opts msg0 xs as] []

-- | converts a string 'Data.Text.Lens.IsText' value to upper case
--
-- >>> pz @ToUpper "HeLlO wOrld!"
-- Present "HELLO WORLD!"
-- PresentT "HELLO WORLD!"
--
data ToUpper

instance (Show a, TL.IsText a) => P ToUpper a where
  type PP ToUpper a = a
  eval _ opts as =
    let msg0 = "ToUpper"
        xs = as & TL.text %~ toUpper
    in pure $ mkNode opts (PresentT xs) [show01 opts msg0 xs as] []


-- | similar to 'Data.List.inits'
--
-- >>> pz @Inits [4,8,3,9]
-- Present [[],[4],[4,8],[4,8,3],[4,8,3,9]]
-- PresentT [[],[4],[4,8],[4,8,3],[4,8,3,9]]
--
-- >>> pz @Inits []
-- Present [[]]
-- PresentT [[]]
--
data Inits

instance Show a => P Inits [a] where
  type PP Inits [a] = [[a]]
  eval _ opts as =
    let msg0 = "Inits"
        xs = inits as
    in pure $ mkNode opts (PresentT xs) [show01 opts msg0 xs as] []

-- | similar to 'Data.List.tails'
--
-- >>> pz @Tails [4,8,3,9]
-- Present [[4,8,3,9],[8,3,9],[3,9],[9],[]]
-- PresentT [[4,8,3,9],[8,3,9],[3,9],[9],[]]
--
-- >>> pz @Tails []
-- Present [[]]
-- PresentT [[]]
--
data Tails

instance Show a => P Tails [a] where
  type PP Tails [a] = [[a]]
  eval _ opts as =
    let msg0 = "Tails"
        xs = tails as
    in pure $ mkNode opts (PresentT xs) [show01 opts msg0 xs as] []

-- | split a list into single values
--
-- >>> pz @(Ones Id) [4,8,3,9]
-- Present [[4],[8],[3],[9]]
-- PresentT [[4],[8],[3],[9]]
--
-- >>> pz @(Ones Id) []
-- Present []
-- PresentT []
--
data Ones p

instance ( PP p x ~ [a]
         , P p x
         , Show a
         ) => P (Ones p) x where
  type PP (Ones p) x = [PP p x]
  eval _ opts x = do
    let msg0 = "Ones"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case chkSize opts msg0 p [hh pp] of
          Left e -> e
          Right () ->
            let d = map (:[]) p
            in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]

-- | similar to 'show'
--
-- >>> pz @(ShowP Id) [4,8,3,9]
-- Present "[4,8,3,9]"
-- PresentT "[4,8,3,9]"
--
-- >>> pz @(ShowP Id) 'x'
-- Present "'x'"
-- PresentT "'x'"
--
-- >>> pz @(ShowP (42 -% 10)) 'x'
-- Present "(-21) % 5"
-- PresentT "(-21) % 5"
--
data ShowP p

instance (Show (PP p x), P p x) => P (ShowP p) x where
  type PP (ShowP p) x = String
  eval _ opts x = do
    let msg0 = "ShowP"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = show p
        in mkNode opts (PresentT d) [msg0 <> showLit0 opts " " d <> show1 opts " | " p] [hh pp]

-- | type level expression representing a formatted time
-- similar to 'Data.Time.formatTime' using a type level 'Symbol' to get the formatting string
--
-- >>> pz @(FormatTimeP "%F %T" Id) (readNote @LocalTime "invalid localtime" "2019-05-24 05:19:59")
-- Present "2019-05-24 05:19:59"
-- PresentT "2019-05-24 05:19:59"
--
-- >>> pz @(FormatTimeP (Fst Id) (Snd Id)) ("the date is %d/%m/%Y", readNote @Day "invalid day" "2019-05-24")
-- Present "the date is 24/05/2019"
-- PresentT "the date is 24/05/2019"
--
data FormatTimeP p q

instance (PP p x ~ String
        , FormatTime (PP q x)
        , P p x
        , Show (PP q x)
        , P q x
        ) => P (FormatTimeP p q) x where
  type PP (FormatTimeP p q) x = String
  eval _ opts x = do
    let msg0 = "FormatTimeP"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            b = formatTime defaultTimeLocale p q
        in mkNode opts (PresentT b) [msg1 <> showLit0 opts " " b <> show1 opts " | " q] [hh pp, hh qq]

-- | similar to 'Data.Time.parseTimeM' where \'t\' is the 'Data.Time.ParseTime' type, \'p\' is the datetime format and \'q\' points to the content to parse
--
-- >>> pz @(ParseTimeP LocalTime "%F %T" Id) "2019-05-24 05:19:59"
-- Present 2019-05-24 05:19:59
-- PresentT 2019-05-24 05:19:59
--
-- >>> pz @(ParseTimeP LocalTime "%F %T" "2019-05-24 05:19:59") (Right "never used")
-- Present 2019-05-24 05:19:59
-- PresentT 2019-05-24 05:19:59
--
-- keeping \'q\' as we might want to extract from a tuple
data ParseTimeP' t p q

instance (ParseTime (PP t a)
        , Typeable (PP t a)
        , Show (PP t a)
        , P p a
        , P q a
        , PP p a ~ String
        , PP q a ~ String
        ) => P (ParseTimeP' t p q) a where
  type PP (ParseTimeP' t p q) a = PP t a
  eval _ opts a = do
    let msg0 = "ParseTimeP " <> t
        t = showT @(PP t a)
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case parseTimeM @Maybe @(PP t a) True defaultTimeLocale p q of
             Just b -> mkNode opts (PresentT b) [lit01' opts msg1 b "fmt=" p <> show1 opts " | " q] hhs
             Nothing -> mkNode opts (FailT (msg1 <> " failed to parse")) [msg1 <> " failed"] hhs

data ParseTimeP (t :: Type) p q
type ParseTimePT (t :: Type) p q = ParseTimeP' (Hole t) p q

instance P (ParseTimePT t p q) x => P (ParseTimeP t p q) x where
  type PP (ParseTimeP t p q) x = PP (ParseTimePT t p q) x
  eval _ = eval (Proxy @(ParseTimePT t p q))

-- | A convenience method to match against many different datetime formats to find a match
--
-- >>> pz @(ParseTimes LocalTime '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"] "03/11/19 01:22:33") ()
-- Present 2019-03-11 01:22:33
-- PresentT 2019-03-11 01:22:33
--
-- >>> pz @(ParseTimes LocalTime (Fst Id) (Snd Id)) (["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"], "03/11/19 01:22:33")
-- Present 2019-03-11 01:22:33
-- PresentT 2019-03-11 01:22:33
--
data ParseTimes' t p q

instance (ParseTime (PP t a)
        , Typeable (PP t a)
        , Show (PP t a)
        , P p a
        , P q a
        , PP p a ~ [String]
        , PP q a ~ String
        ) => P (ParseTimes' t p q) a where
  type PP (ParseTimes' t p q) a = PP t a
  eval _ opts a = do
    let msg0 = "ParseTimes " <> t
        t = showT @(PP t a)
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0
            hhs = [hh pp, hh qq]
            zs = map (\d -> (d,) <$> parseTimeM @Maybe @(PP t a) True defaultTimeLocale d q) p
        in case catMaybes zs of
             [] -> mkNode opts (FailT ("no match on [" ++ q ++ "]")) [msg1 <> " no match"] hhs
             (d,b):_ -> mkNode opts (PresentT b) [lit01' opts msg1 b "fmt=" d <> show1 opts " | " q] hhs

data ParseTimes (t :: Type) p q
type ParseTimesT (t :: Type) p q = ParseTimes' (Hole t) p q

instance P (ParseTimesT t p q) x => P (ParseTimes t p q) x where
  type PP (ParseTimes t p q) x = PP (ParseTimesT t p q) x
  eval _ = eval (Proxy @(ParseTimesT t p q))

-- | create a 'Day' from three int values passed in as year month and day
--
-- >>> pz @MkDay (2019,12,30)
-- Present Just (2019-12-30,1,1)
-- PresentT (Just (2019-12-30,1,1))
--
-- >>> pz @(MkDay' (Fst Id) (Snd Id) (Thd Id)) (2019,99,99999)
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @MkDay (1999,3,13)
-- Present Just (1999-03-13,10,6)
-- PresentT (Just (1999-03-13,10,6))
--
data MkDay' p q r

instance (P p x
        , P q x
        , P r x
        , PP p x ~ Int
        , PP q x ~ Int
        , PP r x ~ Int
        ) => P (MkDay' p q r) x where
  type PP (MkDay' p q r) x = Maybe (Day, Int, Int)
  eval _ opts x = do
    let msg0 = "MkDay"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        rr <- eval (Proxy @r) opts x
        pure $ case getValueLR opts msg0 rr hhs of
          Left e -> e
          Right r ->
            let mday = fromGregorianValid (fromIntegral p) q r
                b = mday <&> \day ->
                      let (_, week, dow) = toWeekDate day
                      in (day, week, dow)
            in mkNode opts (PresentT b) [show01' opts msg0 b "(y,m,d)=" (p,q,r)] (hhs <> [hh rr])

data MkDay
type MkDayT = MkDay' (Fst Id) (Snd Id) (Thd Id)

instance P MkDayT x => P MkDay x where
  type PP MkDay x = PP MkDayT x
  eval _ = eval (Proxy @MkDayT)

-- | uncreate a 'Day' returning year month and day
--
-- >>> pz @(UnMkDay Id) (readNote "invalid day" "2019-12-30")
-- Present (2019,12,30)
-- PresentT (2019,12,30)
--
data UnMkDay p

instance (PP p x ~ Day, P p x) => P (UnMkDay p) x where
  type PP (UnMkDay p) x = (Int, Int, Int)
  eval _ opts x = do
    let msg0 = "UnMkDay"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (fromIntegral -> y, m, d) = toGregorian p
            b = (y, m, d)
        in mkNode opts (PresentT b) [show01 opts msg0 b p] []

-- | uses the 'Read' of the given type \'t\' and \'p\' which points to the content to read
--
-- >>> pz @(ReadP Rational Id) "4 % 5"
-- Present 4 % 5
-- PresentT (4 % 5)
--
-- >>> pz @(ReadP Day Id >> Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30")) "2018-10-12"
-- True
-- TrueT
--
-- >>> pz @(ReadP Day Id >> Between (ReadP Day "2017-04-11") (ReadP Day "2018-12-30")) "2016-10-12"
-- False
-- FalseT
--
data ReadP' t p

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , Read (PP t x)
        ) => P (ReadP' t p) x where
  type PP (ReadP' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ReadP " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,"")] -> mkNode opts (PresentT b) [msg0 <> " " ++ show b] hhs
           o -> mkNode opts (FailT (msg0 <> " (" ++ s ++ ")")) [msg0 <> " failed " <> show o <> " | s=" ++ s] hhs

data ReadP (t :: Type) p
type ReadPT (t :: Type) p = ReadP' (Hole t) p

instance P (ReadPT t p) x => P (ReadP t p) x where
  type PP (ReadP t p) x = PP (ReadPT t p) x
  eval _ = eval (Proxy @(ReadPT t p))


-- [] (a,s) (a,[])

-- | Read but returns the Maybe of the value and any remaining unparsed string
--
-- >>> pz @(ReadMaybe Int Id) "123x"
-- Present Just (123,"x")
-- PresentT (Just (123,"x"))
--
-- >>> pz @(ReadMaybe Int Id) "123"
-- Present Just (123,"")
-- PresentT (Just (123,""))
--
-- >>> pz @(ReadMaybe Int Id) "x123"
-- Present Nothing
-- PresentT Nothing
--
data ReadMaybe' t p

-- not as good as ReadQ
-- type ReadZ' t p = ReadMaybe' t p >> JustFail "read failed" Id >> (Guard "oops" (Snd Id >> Null) >> Fst Id)

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , Read (PP t x)
        ) => P (ReadMaybe' t p) x where
  type PP (ReadMaybe' t p) x = Maybe (PP t x, String)
  eval _ opts x = do
    let msg0 = "ReadMaybe " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let msg1 = msg0 <> " (" <> s <> ")"
            hhs = [hh pp]
        in case reads @(PP t x) s of
           [(b,rest)] -> mkNode opts (PresentT (Just (b,rest))) [lit01 opts msg1 b s] hhs
           o -> mkNode opts (PresentT Nothing) [msg1 <> " failed " <> show o] hhs

data ReadMaybe (t :: Type) p
type ReadMaybeT (t :: Type) p = ReadMaybe' (Hole t) p

instance P (ReadMaybeT t p) x => P (ReadMaybe t p) x where
  type PP (ReadMaybe t p) x = PP (ReadMaybeT t p) x
  eval _ = eval (Proxy @(ReadMaybeT t p))

-- | emulates ReadP
data ReadQ' t p
type ReadQT' t p = ReadMaybe' t p >> MaybeIn (Failp "read failed") (Guard "oops" (Snd Id >> Null) >> Fst Id)

instance P (ReadQT' t p) x => P (ReadQ' t p) x where
  type PP (ReadQ' t p) x = PP (ReadQT' t p) x
  eval _ = eval (Proxy @(ReadQT' t p))

data ReadQ (t :: Type) p
type ReadQT (t :: Type) p = ReadQ' (Hole t) p

instance P (ReadQT t p) x => P (ReadQ t p) x where
  type PP (ReadQ t p) x = PP (ReadQT t p) x
  eval _ = eval (Proxy @(ReadQT t p))

-- | similar to 'sum'
--
-- >>> pz @Sum [10,4,5,12,3,4]
-- Present 38
-- PresentT 38
--
-- >>> pz @Sum []
-- Present 0
-- PresentT 0
--
data Sum

instance (Num a, Show a) => P Sum [a] where
  type PP Sum [a] = a
  eval _ opts as =
    let msg0 = "Sum"
        v = sum as
    in pure $ mkNode opts (PresentT v) [show01 opts msg0 v as] []

-- | similar to 'minimum'
--
-- >>> pz @Min [10,4,5,12,3,4]
-- Present 3
-- PresentT 3
--
-- >>> pz @Min []
-- Error empty list
-- FailT "empty list"
--
data Min

instance (Ord a, Show a) => P Min [a] where
  type PP Min [a] = a
  eval _ opts as' = do
    let msg0 = "Min"
    pure $ case as' of
     [] -> mkNode opts (FailT "empty list") [msg0 <> "(empty list)"] []
     as@(_:_) ->
       let v = minimum as
       in mkNode opts (PresentT v) [show01 opts msg0 v as] []

-- | similar to 'maximum'
--
-- >>> pz @Max [10,4,5,12,3,4]
-- Present 12
-- PresentT 12
--
-- >>> pz @Max []
-- Error empty list
-- FailT "empty list"
--

data Max

instance (Ord a, Show a) => P Max [a] where
  type PP Max [a] = a
  eval _ opts as' = do
    let msg0 = "Max"
    pure $ case as' of
      [] -> mkNode opts (FailT "empty list") [msg0 <> "(empty list)"] []
      as@(_:_) ->
        let v = maximum as
        in mkNode opts (PresentT v) [show01 opts msg0 v as] []

-- | sort a list
--
-- >>> pz @(SortOn (Fst Id) Id) [(10,"abc"), (3,"def"), (4,"gg"), (10,"xyz"), (1,"z")]
-- Present [(1,"z"),(3,"def"),(4,"gg"),(10,"abc"),(10,"xyz")]
-- PresentT [(1,"z"),(3,"def"),(4,"gg"),(10,"abc"),(10,"xyz")]
--
-- >>> pz @(SortBy (OrdP (Snd Id) (Fst Id)) Id) [(10,"ab"),(4,"x"),(20,"bbb")]
-- Present [(20,"bbb"),(10,"ab"),(4,"x")]
-- PresentT [(20,"bbb"),(10,"ab"),(4,"x")]
--
-- >>> pz @(SortBy 'LT Id) [1,5,2,4,7,0]
-- Present [1,5,2,4,7,0]
-- PresentT [1,5,2,4,7,0]
--
-- >>> pz @(SortBy 'GT Id) [1,5,2,4,7,0]
-- Present [0,7,4,2,5,1]
-- PresentT [0,7,4,2,5,1]
--
-- >>> pz @(SortBy ((Fst (Fst Id) ==! Fst (Snd Id)) <> (Snd (Fst Id) ==! Snd (Snd Id))) Id) [(10,"ab"),(4,"x"),(20,"bbb"),(4,"a"),(4,"y")]
-- Present [(4,"a"),(4,"x"),(4,"y"),(10,"ab"),(20,"bbb")]
-- PresentT [(4,"a"),(4,"x"),(4,"y"),(10,"ab"),(20,"bbb")]
--
-- >>> pz @(SortBy ((Fst (Fst Id) ==! Fst (Snd Id)) <> (Snd (Snd Id) ==! Snd (Fst Id))) Id) [(10,"ab"),(4,"x"),(20,"bbb"),(4,"a"),(4,"y")]
-- Present [(4,"y"),(4,"x"),(4,"a"),(10,"ab"),(20,"bbb")]
-- PresentT [(4,"y"),(4,"x"),(4,"a"),(10,"ab"),(20,"bbb")]
--
data SortBy p q

type SortByHelperT p = Partition (p == 'GT) Id

instance (P p (a,a)
        , P q x
        , Show a
        , PP q x ~ [a]
        , PP p (a,a) ~ Ordering
        ) => P (SortBy p q) x where
  type PP (SortBy p q) x = PP q x
  eval _ opts x = do
    let msg0 = "SortBy"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts (msg0 <> " q failed") qq [] of
      Left e -> pure e
      Right as -> do
        let ff :: MonadEval m => [a] -> m (TT [a])
            ff = \case
                [] -> pure $ mkNode opts (PresentT mempty) [msg0 <> " empty"] []
                [w] -> pure $ mkNode opts (PresentT [w]) [msg0 <> " one element " <> show w] []
                w:ys@(_:_) -> do
                  pp <- (if isVerbose opts then
                              eval (Proxy @(SortByHelperT p))
                         else eval (Proxy @(Hide (SortByHelperT p)))) opts (map (w,) ys)
--                  pp <- eval (Proxy @(Hide (Partition (p >> Id == 'GT) Id))) opts (map (w,) ys)
-- too much output: dont need (Map (Snd Id) *** Map (Snd Id)) -- just do map snd in code
--                  pp <- eval (Proxy @(Partition (p >> (Id == 'GT)) Id >> (Map (Snd Id) *** Map (Snd Id)))) opts (map (w,) ys)
                  case getValueLR opts msg0 pp [] of
                    Left e -> pure e
                    Right (ll', rr') -> do
                      lhs <- ff (map snd ll')
                      case getValueLR opts msg0 lhs [] of
                        Left _ -> pure lhs -- dont rewrap
                        Right ll -> do
                          rhs <- ff (map snd rr')
                          case getValueLR opts msg0 rhs [] of
                            Left _ -> pure rhs
                            Right rr -> do
                              pure $  mkNode opts (PresentT (ll ++ w : rr))
                                     [msg0 <> show0 opts " lhs=" ll <> " pivot " <> show w <> show0 opts " rhs=" rr]
                                     (hh pp : [hh lhs | length ll > 1] ++ [hh rhs | length rr > 1])
        ret <- ff as
        pure $ case getValueLR opts msg0 ret [hh qq] of
          Left _e -> ret -- dont rewrap else will double up messages: already handled
          Right xs -> mkNode opts (_tBool ret) [msg0 <> show0 opts " " xs] [hh qq, hh ret]

data SortOn p q
type SortOnT p q = SortBy (OrdA p) q

instance P (SortOnT p q) x => P (SortOn p q) x where
  type PP (SortOn p q) x = PP (SortOnT p q) x
  eval _ = eval (Proxy @(SortOnT p q))

data SortOnDesc p q
type SortOnDescT p q = SortBy (Swap >> OrdA p) q

instance P (SortOnDescT p q) x => P (SortOnDesc p q) x where
  type PP (SortOnDesc p q) x = PP (SortOnDescT p q) x
  eval _ = eval (Proxy @(SortOnDescT p q))

-- | similar to 'length'
--
-- >>> pz @Len [10,4,5,12,3,4]
-- Present 6
-- PresentT 6
--
-- >>> pz @Len []
-- Present 0
-- PresentT 0
--
data Len
instance (Show a, as ~ [a]) => P Len as where
  type PP Len as = Int
  eval _ opts as =
    let msg0 = "Len"
        n = length as
    in pure $ mkNode opts (PresentT n) [show01 opts msg0 n as] []

-- | similar to 'length' for 'Foldable' instances
--
-- >>> pz @(Length Id) (Left "aa")
-- Present 0
-- PresentT 0
--
-- >>> pz @(Length Id) (Right "aa")
-- Present 1
-- PresentT 1
--
-- >>> pz @(Length (Right' Id)) (Right "abcd")
-- Present 4
-- PresentT 4
--
-- >>> pz @(Length (Thd (Snd Id))) (True,(23,'x',[10,9,1,3,4,2]))
-- Present 6
-- PresentT 6
--
data Length p

instance (PP p x ~ t a
        , P p x
        , Show (t a)
        , Foldable t) => P (Length p) x where
  type PP (Length p) x = Int
  eval _ opts x = do
    let msg0 = "Length"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
            let n = length p
            in mkNode opts (PresentT n) [show01 opts msg0 n p] []

-- | similar to 'fst'
--
-- >>> pz @(Fst Id) (10,"Abc")
-- Present 10
-- PresentT 10
--
-- >>> pz @(Fst Id) (10,"Abc",'x')
-- Present 10
-- PresentT 10
--
-- >>> pz @(Fst Id) (10,"Abc",'x',False)
-- Present 10
-- PresentT 10
--
data Fst p

instance (Show (ExtractL1T (PP p x))
        , ExtractL1C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Fst p) x where
  type PP (Fst p) x = ExtractL1T (PP p x)
  eval _ opts x = do
    let msg0 = "Fst"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL1C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data L1 p
type L1T p = Fst p

instance P (L1T p) x => P (L1 p) x where
  type PP (L1 p) x = PP (L1T p) x
  eval _ = eval (Proxy @(L1T p))

class ExtractL1C tp where
  type ExtractL1T tp
  extractL1C :: tp -> ExtractL1T tp
instance ExtractL1C (a,b) where
  type ExtractL1T (a,b) = a
  extractL1C (a,_) = a
instance ExtractL1C (a,b,c) where
  type ExtractL1T (a,b,c) = a
  extractL1C (a,_,_) = a
instance ExtractL1C (a,b,c,d) where
  type ExtractL1T (a,b,c,d) = a
  extractL1C (a,_,_,_) = a
instance ExtractL1C (a,b,c,d,e) where
  type ExtractL1T (a,b,c,d,e) = a
  extractL1C (a,_,_,_,_) = a
instance ExtractL1C (a,b,c,d,e,f) where
  type ExtractL1T (a,b,c,d,e,f) = a
  extractL1C (a,_,_,_,_,_) = a

-- | similar to 'snd'
--
-- >>> pz @(Snd Id) (10,"Abc")
-- Present "Abc"
-- PresentT "Abc"
--
-- >>> pz @(Snd Id) (10,"Abc",True)
-- Present "Abc"
-- PresentT "Abc"
--
data Snd p

instance (Show (ExtractL2T (PP p x))
        , ExtractL2C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Snd p) x where
  type PP (Snd p) x = ExtractL2T (PP p x)
  eval _ opts x = do
    let msg0 = "Snd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL2C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data L2 p
type L2T p = Snd p

instance P (L2T p) x => P (L2 p) x where
  type PP (L2 p) x = PP (L2T p) x
  eval _ = eval (Proxy @(L2T p))

class ExtractL2C tp where
  type ExtractL2T tp
  extractL2C :: tp -> ExtractL2T tp
instance ExtractL2C (a,b) where
  type ExtractL2T (a,b) = b
  extractL2C (_,b) = b
instance ExtractL2C (a,b,c) where
  type ExtractL2T (a,b,c) = b
  extractL2C (_,b,_) = b
instance ExtractL2C (a,b,c,d) where
  type ExtractL2T (a,b,c,d) = b
  extractL2C (_,b,_,_) = b
instance ExtractL2C (a,b,c,d,e) where
  type ExtractL2T (a,b,c,d,e) = b
  extractL2C (_,b,_,_,_) = b
instance ExtractL2C (a,b,c,d,e,f) where
  type ExtractL2T (a,b,c,d,e,f) = b
  extractL2C (_,b,_,_,_,_) = b

-- | similar to 3rd element in a n-tuple
--
-- >>> pz @(Thd Id) (10,"Abc",133)
-- Present 133
-- PresentT 133
--
-- >>> pz @(Thd Id) (10,"Abc",133,True)
-- Present 133
-- PresentT 133
--
data Thd p

instance (Show (ExtractL3T (PP p x))
        , ExtractL3C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (Thd p) x where
  type PP (Thd p) x = ExtractL3T (PP p x)
  eval _ opts x = do
    let msg0 = "Thd"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL3C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data L3 p
type L3T p = Thd p

instance P (L3T p) x => P (L3 p) x where
  type PP (L3 p) x = PP (L3T p) x
  eval _ = eval (Proxy @(L3T p))

class ExtractL3C tp where
  type ExtractL3T tp
  extractL3C :: tp -> ExtractL3T tp
instance ExtractL3C (a,b) where
  type ExtractL3T (a,b) = GL.TypeError ('GL.Text "Thd doesn't work for 2-tuples")
  extractL3C _ = errorInProgram "Thd doesn't work for 2-tuples"
instance ExtractL3C (a,b,c) where
  type ExtractL3T (a,b,c) = c
  extractL3C (_,_,c) = c
instance ExtractL3C (a,b,c,d) where
  type ExtractL3T (a,b,c,d) = c
  extractL3C (_,_,c,_) = c
instance ExtractL3C (a,b,c,d,e) where
  type ExtractL3T (a,b,c,d,e) = c
  extractL3C (_,_,c,_,_) = c
instance ExtractL3C (a,b,c,d,e,f) where
  type ExtractL3T (a,b,c,d,e,f) = c
  extractL3C (_,_,c,_,_,_) = c

-- | similar to 4th element in a n-tuple
--
-- >>> pz @(L4 Id) (10,"Abc",'x',True)
-- Present True
-- PresentT True
--
-- >>> pz @(L4 (Fst (Snd Id))) ('x',((10,"Abc",'x',999),"aa",1),9)
-- Present 999
-- PresentT 999
--
data L4 p

instance (Show (ExtractL4T (PP p x))
        , ExtractL4C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L4 p) x where
  type PP (L4 p) x = ExtractL4T (PP p x)
  eval _ opts x = do
    let msg0 = "L4"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL4C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

class ExtractL4C tp where
  type ExtractL4T tp
  extractL4C :: tp -> ExtractL4T tp
instance ExtractL4C (a,b) where
  type ExtractL4T (a,b) = GL.TypeError ('GL.Text "L4 doesn't work for 2-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 2-tuples"
instance ExtractL4C (a,b,c) where
  type ExtractL4T (a,b,c) = GL.TypeError ('GL.Text "L4 doesn't work for 3-tuples")
  extractL4C _ = errorInProgram "L4 doesn't work for 3-tuples"
instance ExtractL4C (a,b,c,d) where
  type ExtractL4T (a,b,c,d) = d
  extractL4C (_,_,_,d) = d
instance ExtractL4C (a,b,c,d,e) where
  type ExtractL4T (a,b,c,d,e) = d
  extractL4C (_,_,_,d,_) = d
instance ExtractL4C (a,b,c,d,e,f) where
  type ExtractL4T (a,b,c,d,e,f) = d
  extractL4C (_,_,_,d,_,_) = d

-- | similar to 5th element in a n-tuple
--
-- >>> pz @(L5 Id) (10,"Abc",'x',True,1)
-- Present 1
-- PresentT 1
--
data L5 p

instance (Show (ExtractL5T (PP p x))
        , ExtractL5C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L5 p) x where
  type PP (L5 p) x = ExtractL5T (PP p x)
  eval _ opts x = do
    let msg0 = "L5"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL5C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

class ExtractL5C tp where
  type ExtractL5T tp
  extractL5C :: tp -> ExtractL5T tp
instance ExtractL5C (a,b) where
  type ExtractL5T (a,b) = GL.TypeError ('GL.Text "L5 doesn't work for 2-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 2-tuples"
instance ExtractL5C (a,b,c) where
  type ExtractL5T (a,b,c) = GL.TypeError ('GL.Text "L5 doesn't work for 3-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 3-tuples"
instance ExtractL5C (a,b,c,d) where
  type ExtractL5T (a,b,c,d) = GL.TypeError ('GL.Text "L5 doesn't work for 4-tuples")
  extractL5C _ = errorInProgram "L5 doesn't work for 4-tuples"
instance ExtractL5C (a,b,c,d,e) where
  type ExtractL5T (a,b,c,d,e) = e
  extractL5C (_,_,_,_,e) = e
instance ExtractL5C (a,b,c,d,e,f) where
  type ExtractL5T (a,b,c,d,e,f) = e
  extractL5C (_,_,_,_,e,_) = e


-- | similar to 6th element in a n-tuple
--
-- >>> pz @(L6 Id) (10,"Abc",'x',True,1,99)
-- Present 99
-- PresentT 99
--
data L6 p

instance (Show (ExtractL6T (PP p x))
        , ExtractL6C (PP p x)
        , P p x
        , Show (PP p x)
        ) => P (L6 p) x where
  type PP (L6 p) x = ExtractL6T (PP p x)
  eval _ opts x = do
    let msg0 = "L6"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = extractL6C p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

class ExtractL6C tp where
  type ExtractL6T tp
  extractL6C :: tp -> ExtractL6T tp
instance ExtractL6C (a,b) where
  type ExtractL6T (a,b) = GL.TypeError ('GL.Text "L6 doesn't work for 2-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 2-tuples"
instance ExtractL6C (a,b,c) where
  type ExtractL6T (a,b,c) = GL.TypeError ('GL.Text "L6 doesn't work for 3-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 3-tuples"
instance ExtractL6C (a,b,c,d) where
  type ExtractL6T (a,b,c,d) = GL.TypeError ('GL.Text "L6 doesn't work for 4-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 4-tuples"
instance ExtractL6C (a,b,c,d,e) where
  type ExtractL6T (a,b,c,d,e) = GL.TypeError ('GL.Text "L6 doesn't work for 5-tuples")
  extractL6C _ = errorInProgram "L6 doesn't work for 5-tuples"
instance ExtractL6C (a,b,c,d,e,f) where
  type ExtractL6T (a,b,c,d,e,f) = f
  extractL6C (_,_,_,_,_,f) = f


-- | 'fromString' function where you need to provide the type \'t\' of the result
--
-- >>> :set -XFlexibleContexts
-- >>> pz @(FromStringP (Identity _) Id) "abc"
-- Present Identity "abc"
-- PresentT (Identity "abc")
--
-- >>> pz @(FromStringP (Seq.Seq Char) Id) "abc"
-- Present fromList "abc"
-- PresentT (fromList "abc")
data FromStringP' t s

instance (P s a
        , PP s a ~ String
        , Show (PP t a)
        , IsString (PP t a)
        ) => P (FromStringP' t s) a where
  type PP (FromStringP' t s) a = PP t a
  eval _ opts a = do
    let msg0 = "FromStringP"
    ss <- eval (Proxy @s) opts a
    pure $ case getValueLR opts msg0 ss [] of
      Left e -> e
      Right s ->
        let b = fromString @(PP t a) s
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b] [hh ss]

data FromStringP (t :: Type) p
type FromStringPT (t :: Type) p = FromStringP' (Hole t) p

instance P (FromStringPT t p) x => P (FromStringP t p) x where
  type PP (FromStringP t p) x = PP (FromStringPT t p) x
  eval _ = eval (Proxy @(FromStringPT t p))


-- | 'fromInteger' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromInteger (SG.Sum _) Id) 23
-- Present Sum {getSum = 23}
-- PresentT (Sum {getSum = 23})
--
-- >>> pz @(FromInteger Rational 44) 12
-- Present 44 % 1
-- PresentT (44 % 1)
--
-- >>> pz @(FromInteger Rational Id) 12
-- Present 12 % 1
-- PresentT (12 % 1)
--
data FromInteger' t n

instance (Num (PP t a)
        , Integral (PP n a)
        , P n a
        , Show (PP t a)
        ) => P (FromInteger' t n) a where
  type PP (FromInteger' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromInteger"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromInteger (fromIntegral n)
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b] [hh nn]

data FromInteger (t :: Type) p
type FromIntegerT (t :: Type) p = FromInteger' (Hole t) p
--type FromIntegerP n = FromInteger' Unproxy n

instance P (FromIntegerT t p) x => P (FromInteger t p) x where
  type PP (FromInteger t p) x = PP (FromIntegerT t p) x
  eval _ = eval (Proxy @(FromIntegerT t p))

-- | 'fromIntegral' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromIntegral (SG.Sum _) Id) 23
-- Present Sum {getSum = 23}
-- PresentT (Sum {getSum = 23})
data FromIntegral' t n

instance (Num (PP t a)
        , Integral (PP n a)
        , P n a
        , Show (PP t a)
        , Show (PP n a)
        ) => P (FromIntegral' t n) a where
  type PP (FromIntegral' t n) a = PP t a
  eval _ opts a = do
    let msg0 = "FromIntegral"
    nn <- eval (Proxy @n) opts a
    pure $ case getValueLR opts msg0 nn [] of
      Left e -> e
      Right n ->
        let b = fromIntegral n
        in mkNode opts (PresentT b) [show01 opts msg0 b n] [hh nn]

data FromIntegral (t :: Type) p
type FromIntegralT (t :: Type) p = FromIntegral' (Hole t) p

instance P (FromIntegralT t p) x => P (FromIntegral t p) x where
  type PP (FromIntegral t p) x = PP (FromIntegralT t p) x
  eval _ = eval (Proxy @(FromIntegralT t p))

-- | 'toRational' function
--
-- >>> pz @(ToRational Id) 23.5
-- Present 47 % 2
-- PresentT (47 % 2)

data ToRational p

instance (a ~ PP p x
         , Show a
         , Real a
         , P p x)
   => P (ToRational p) x where
  type PP (ToRational p) x = Rational
  eval _ opts x = do
    let msg0 = "ToRational"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right a ->
        let r = (toRational a)
        in mkNode opts (PresentT r) [show01 opts msg0 r a] [hh pp]

-- | 'fromRational' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(FromRational Rational Id) 23.5
-- Present 47 % 2
-- PresentT (47 % 2)
data FromRational' t r

instance (P r a
        , PP r a ~ Rational
        , Show (PP t a)
        , Fractional (PP t a)
        ) => P (FromRational' t r) a where
  type PP (FromRational' t r) a = PP t a
  eval _ opts a = do
    let msg0 = "FromRational"
    rr <- eval (Proxy @r) opts a
    pure $ case getValueLR opts msg0 rr [] of
      Left e -> e
      Right r ->
        let b = fromRational @(PP t a) r
        in mkNode opts (PresentT b) [show01 opts msg0 b r] [hh rr]

data FromRational (t :: Type) p
type FromRationalT (t :: Type) p = FromRational' (Hole t) p

instance P (FromRationalT t p) x => P (FromRational t p) x where
  type PP (FromRational t p) x = PP (FromRationalT t p) x
  eval _ = eval (Proxy @(FromRationalT t p))

-- | 'truncate' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Truncate Int Id) (23 % 5)
-- Present 4
-- PresentT 4
data Truncate' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Truncate' t p) x where
  type PP (Truncate' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Truncate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = truncate p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data Truncate (t :: Type) p
type TruncateT (t :: Type) p = Truncate' (Hole t) p

instance P (TruncateT t p) x => P (Truncate t p) x where
  type PP (Truncate t p) x = PP (TruncateT t p) x
  eval _ = eval (Proxy @(TruncateT t p))

-- | 'ceiling' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Ceiling Int Id) (23 % 5)
-- Present 5
-- PresentT 5
data Ceiling' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Ceiling' t p) x where
  type PP (Ceiling' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Ceiling"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = ceiling p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data Ceiling (t :: Type) p
type CeilingT (t :: Type) p = Ceiling' (Hole t) p

instance P (CeilingT t p) x => P (Ceiling t p) x where
  type PP (Ceiling t p) x = PP (CeilingT t p) x
  eval _ = eval (Proxy @(CeilingT t p))

-- | 'floor' function where you need to provide the type \'t\' of the result
--
-- >>> pz @(Floor Int Id) (23 % 5)
-- Present 4
-- PresentT 4
data Floor' t p

instance (Show (PP p x)
        , P p x
        , Show (PP t x)
        , RealFrac (PP p x)
        , Integral (PP t x)
        ) => P (Floor' t p) x where
  type PP (Floor' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "Floor"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = floor p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data Floor (t :: Type) p
type FloorT (t :: Type) p = Floor' (Hole t) p

instance P (FloorT t p) x => P (Floor t p) x where
  type PP (Floor t p) x = PP (FloorT t p) x
  eval _ = eval (Proxy @(FloorT t p))
-- | converts a value to a 'Proxy': the same as '\'Proxy'
--
-- >>> pz @MkProxy 'x'
-- Present Proxy
-- PresentT Proxy
--
data MkProxy

instance Show a => P MkProxy a where
  type PP MkProxy a = Proxy a
  eval _ opts a =
    let msg0 = "MkProxy"
        b = Proxy @a
    in pure $ mkNode opts (PresentT b) [msg0 <> show1 opts " | " a] []

-- | processes a type level list predicates running each in sequence: see 'Predicate.>>'
--
-- >>> pz @(Do [Pred Id, ShowP Id, Id &&& Len]) 9876543
-- Present ("9876542",7)
-- PresentT ("9876542",7)
--
-- >>> pz @(Do '[W 123, W "xyz", Len &&& Id, Pred Id *** Id<>Id]) ()
-- Present (2,"xyzxyz")
-- PresentT (2,"xyzxyz")
--
data Do (ps :: [k])

instance (P (DoExpandT ps) a) => P (Do ps) a where
  type PP (Do ps) a = PP (DoExpandT ps) a
  eval _ = eval (Proxy @(DoExpandT ps))

type family DoExpandT (ps :: [k]) :: Type where
  DoExpandT '[] = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  DoExpandT '[p] = Id >> p -- need this else fails cos 1 is nat and would mean that the result is nat not Type!
  -- if p >> Id then turns TrueT to PresentT True
  DoExpandT (p ': p1 ': ps) = p >> DoExpandT (p1 ': ps)

-- | Convenient method to convert a value \'p\' to a 'Maybe' based on a predicate '\b\'
-- if '\b\' then Just \'p'\ else Nothing
--
-- >>> pz @(MaybeBool (Id > 4) Id) 24
-- Present Just 24
-- PresentT (Just 24)
--
-- >>> pz @(MaybeBool (Id > 4) Id) (-5)
-- Present Nothing
-- PresentT Nothing
--
data MaybeBool b p

instance (Show (PP p a)
        , P b a
        , P p a
        , PP b a ~ Bool
        ) => P (MaybeBool b p) a where
  type PP (MaybeBool b p) a = Maybe (PP p a)
  eval _ opts z = do
    let msg0 = "MaybeBool"
    bb <- evalBool (Proxy @b) opts z
    case getValueLR opts (msg0 <> " b failed") bb [] of
      Left e -> pure e
      Right True -> do
        pp <- eval (Proxy @p) opts z
        pure $ case getValueLR opts (msg0 <> " p failed") pp [hh bb] of
          Left e -> e
          Right p -> mkNode opts (PresentT (Just p)) [msg0 <> "(False)" <> show0 opts " Just " p] [hh bb, hh pp]
      Right False -> pure $ mkNode opts (PresentT Nothing) [msg0 <> "(True)"] [hh bb]

-- | Convenient method to convert a \'p\' or '\q'\ to a 'Either' based on a predicate '\b\'
-- if \'b\' then Right \'p\' else Left '\q\'
--
-- >>> pz @(EitherBool (Fst Id > 4) (Snd Id >> Fst Id) (Snd Id >> Snd Id)) (24,(-1,999))
-- Present Right 999
-- PresentT (Right 999)
--
-- >>> pz @(EitherBool (Fst Id > 4) (Fst (Snd Id)) (Snd (Snd Id))) (1,(-1,999))
-- Present Left (-1)
-- PresentT (Left (-1))
--
data EitherBool b p q

instance (Show (PP p a)
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
    case getValueLR opts (msg0 <> " b failed") bb [] of
      Left e -> pure e
      Right False -> do
        pp <- eval (Proxy @p) opts z
        pure $ case getValueLR opts (msg0 <> " p failed") pp [hh bb] of
          Left e -> e
          Right p -> mkNode opts (PresentT (Left p)) [msg0 <> "(False)" <> show0 opts " Left " p] [hh bb, hh pp]
      Right True -> do
        qq <- eval (Proxy @q) opts z
        pure $ case getValueLR opts (msg0 <> " q failed") qq [hh bb] of
          Left e -> e
          Right q -> mkNode opts (PresentT (Right q)) [msg0 <> "(True)" <> show0 opts " Right " q] [hh bb, hh qq]

-- | pad \'q\' with '\n'\ values from '\p'\
--
-- >>> pz @(PadL 5 999 Id) [12,13]
-- Present [999,999,999,12,13]
-- PresentT [999,999,999,12,13]
--
-- >>> pz @(PadR 5 (Fst Id) '[12,13]) (999,'x')
-- Present [12,13,999,999,999]
-- PresentT [12,13,999,999,999]
--
-- >>> pz @(PadR 2 (Fst Id) '[12,13,14]) (999,'x')
-- Present [12,13,14]
-- PresentT [12,13,14]
--
data PadImpl (left :: Bool) n p q

instance (P n a
        , GetBool left
        , Integral (PP n a)
        , [PP p a] ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        ) => P (PadImpl left n p q) a where
  type PP (PadImpl left n p q) a = PP q a
  eval _ opts a = do
    let msg0 = "Pad" <> (if lft then "L" else "R")
        lft = getBool @left
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a []
    case lr of
      Left e -> pure e
      Right (fromIntegral -> n,p,nn,pp) -> do
        let msg1 = msg0 <> show0 opts " " n <> " pad=" <> show p
            hhs = [hh nn, hh pp]
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts (msg1 <> " q failed") qq hhs of
          Left e -> e
          Right q ->
            let l = length q
                diff = if n<=l then 0 else n-l
                bs = if lft
                     then (replicate diff p) <> q
                     else q <> (replicate diff p)
            in mkNode opts (PresentT bs) [show01 opts msg1 bs q] (hhs <> [hh qq])

data PadL n p q
type PadLT n p q = PadImpl 'True n p q

instance P (PadLT n p q) x => P (PadL n p q) x where
  type PP (PadL n p q) x = PP (PadLT n p q) x
  eval _ = eval (Proxy @(PadLT n p q))

data PadR n p q
type PadRT n p q = PadImpl 'False n p q

instance P (PadRT n p q) x => P (PadR n p q) x where
  type PP (PadR n p q) x = PP (PadRT n p q) x
  eval _ = eval (Proxy @(PadRT n p q))

-- | split a list \'p\' into parts using the lengths in the type level list \'ns\'
--
-- >>> pz @(SplitAts '[2,3,1,1] Id) "hello world"
-- Present ["he","llo"," ","w","orld"]
-- PresentT ["he","llo"," ","w","orld"]
--
-- >>> pz @(SplitAts '[2] Id) "hello world"
-- Present ["he","llo world"]
-- PresentT ["he","llo world"]
--
-- >>> pz @(SplitAts '[10,1,1,5] Id) "hello world"
-- Present ["hello worl","d","",""]
-- PresentT ["hello worl","d","",""]
--
data SplitAts ns p

instance (P ns x
        , P p x
        , PP p x ~ [a]
        , Show n
        , Show a
        , PP ns x ~ [n]
        , Integral n
        ) => P (SplitAts ns p) x where
  type PP (SplitAts ns p) x = [PP p x]
  eval _ opts x = do
    let msg0 = "SplitAts"
    lr <- runPQ msg0 (Proxy @ns) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (ns,p,nn,pp) ->
        let zs = foldr (\n k s -> let (a,b) = splitAt (fromIntegral n) s
                              in a:k b
                   ) (\as -> if null as then [] else [as]) ns p
        in mkNode opts (PresentT zs) [show01' opts msg0 zs "ns=" ns <> show1 opts " | " p] [hh nn, hh pp]

-- | similar to 'splitAt'
--
-- >>> pz @(SplitAt 4 Id) "hello world"
-- Present ("hell","o world")
-- PresentT ("hell","o world")
--
-- >>> pz @(SplitAt 20 Id) "hello world"
-- Present ("hello world","")
-- PresentT ("hello world","")
--
-- >>> pz @(SplitAt 0 Id) "hello world"
-- Present ("","hello world")
-- PresentT ("","hello world")
--
-- >>> pz @(SplitAt (Snd Id) (Fst Id)) ("hello world",4)
-- Present ("hell","o world")
-- PresentT ("hell","o world")
--
data SplitAt n p

instance (PP p a ~ [b]
        , P n a
        , P p a
        , Show b
        , Integral (PP n a)
        ) => P (SplitAt n p) a where
  type PP (SplitAt n p) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "SplitAt"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a []
    pure $ case lr of
      Left e -> e -- (Left e, tt')
      Right (fromIntegral -> n,p,pp,qq) ->
        let msg1 = msg0 <> show0 opts " " n <> show0 opts " " p
            (x,y) = splitAt n p
            ret = (x,y)
       in mkNode opts (PresentT ret) [show01' opts msg1 ret "n=" n <> show1 opts " | " p] [hh pp, hh qq]

data Take n p
type TakeT n p = Fst (SplitAt n p)

instance P (TakeT n p) x => P (Take n p) x where
  type PP (Take n p) x = PP (TakeT n p) x
  eval _ = eval (Proxy @(TakeT n p))

data Drop n p
type DropT n p = Snd (SplitAt n p)

instance P (DropT n p) x => P (Drop n p) x where
  type PP (Drop n p) x = PP (DropT n p) x
  eval _ = eval (Proxy @(DropT n p))

--type Tail = Uncons >> 'Just (Snd Id)
--type Head = Uncons >> 'Just (Fst Id)
--type Init = Unsnoc >> 'Just (Fst Id)
--type Last = Unsnoc >> 'Just (Snd Id)

-- | similar to 'Control.Arrow.&&&'
data p &&& q
infixr 3 &&&
type WAmpT p q = W '(p, q)

instance P (WAmpT p q) x => P (p &&& q) x where
  type PP (p &&& q) x = PP (WAmpT p q) x
  eval _ = eval (Proxy @(WAmpT p q))

-- | similar to 'Control.Arrow.***'
--
-- >>> pz @(Pred Id *** ShowP Id) (13, True)
-- Present (12,"True")
-- PresentT (12,"True")
--
-- >>> pl @(FlipT (***) Len (Id * 12)) (99,"cdef")
-- Present (1188,4) ((***) (1188,4) | (99,"cdef"))
-- PresentT (1188,4)
--
data p *** q
infixr 3 ***

instance (Show (PP p a)
        , Show (PP q b)
        , P p a
        , P q b
        , Show a
        , Show b
        ) => P (p *** q) (a,b) where
  type PP (p *** q) (a,b) = (PP p a, PP q b)
  eval _ opts (a,b) = do
    let msg0 = "(***)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right a1 -> do
        qq <- eval (Proxy @q) opts b
        pure $ case getValueLR opts msg0 qq [hh pp] of
          Left e -> e
          Right b1 -> mkNode opts (PresentT (a1,b1)) [msg0 <> show0 opts " " (a1,b1) <> show1 opts " | " (a,b)] [hh pp, hh qq]

data First p
type FirstT p = p *** I

instance P (FirstT p) x => P (First p) x where
  type PP (First p) x = PP (FirstT p) x
  eval _ = eval (Proxy @(FirstT p))

data Second q
type SecondT q = I *** q

instance P (SecondT q) x => P (Second q) x where
  type PP (Second q) x = PP (SecondT q) x
  eval _ = eval (Proxy @(SecondT q))

-- | similar 'Control.Arrow.|||'
--
-- >>> pz @(Pred Id ||| Id) (Left 13)
-- Present 12
-- PresentT 12
--
-- >>> pz @(ShowP Id ||| Id) (Right "hello")
-- Present "hello"
-- PresentT "hello"
--
data p ||| q
infixr 2 |||
type EitherIn p q = p ||| q

instance (Show (PP p a)
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
        pure $ case getValueLR opts msg0 pp [] of
          Left e -> e
          Right a1 -> let msg1 = msg0 ++ " Left"
                      in mkNode opts (_tBool pp) [show01 opts msg1 a1 a] [hh pp]
      Right a -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Right"
            in mkNode opts (_tBool qq) [show01 opts msg1 a1 a] [hh qq]

data IsLeft
type IsLeftT = 'True ||| 'False

instance P IsLeftT x => P IsLeft x where
  type PP IsLeft x = PP IsLeftT x
  eval _ = eval (Proxy @IsLeftT)

data IsRight
type IsRightT = 'False ||| 'True

instance P IsRightT x => P IsRight x where
  type PP IsRight x = PP IsRightT x
  eval _ = eval (Proxy @IsRightT)

-- | similar 'Control.Arrow.+++'
--
-- >>> pz @(Pred Id +++ Id) (Left 13)
-- Present Left 12
-- PresentT (Left 12)
--
-- >>> pz @(ShowP Id +++ Reverse) (Right "hello")
-- Present Right "olleh"
-- PresentT (Right "olleh")
--
data p +++ q
infixr 2 +++

instance (Show (PP p a)
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
        pure $ case getValueLR opts msg0 pp [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Left"
            in mkNode opts (PresentT (Left a1)) [msg1 <> show0 opts " " a1 <> show1 opts " | " a] [hh pp]
      Right a -> do
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg0 qq [] of
          Left e -> e
          Right a1 ->
            let msg1 = msg0 ++ " Right"
            in mkNode opts (PresentT (Right a1)) [msg1 <> show0 opts " " a1 <> show1 opts " | " a] [hh qq]

data Dup
type DupT = W '(Id, Id)

instance Show x => P Dup x where
  type PP Dup x = PP DupT x
  eval _ = eval (Proxy @DupT)

data BinOp = BMult | BSub | BAdd deriving (Show,Eq)

data p + q
infixl 6 +

type AddT p q = Bin 'BAdd p q

instance P (AddT p q) x => P (p + q) x where
  type PP (p + q) x = PP (AddT p q) x
  eval _ = eval (Proxy @(AddT p q))

data p - q
infixl 6 -

type SubT p q = Bin 'BSub p q

instance P (SubT p q) x => P (p - q) x where
  type PP (p - q) x = PP (SubT p q) x
  eval _ = eval (Proxy @(SubT p q))

data p * q
infixl 7 *

type MultT p q = Bin 'BMult p q

instance P (MultT p q) x => P (p * q) x where
  type PP (p * q) x = PP (MultT p q) x
  eval _ = eval (Proxy @(MultT p q))

data p > q
infix 4 >

instance P (Cmp 'CGt p q) x => P (p > q) x where
  type PP (p > q) x = PP (Cmp 'CGt p q) x
  eval _ = eval (Proxy @(Cmp 'CGt p q))

data p >= q
infix 4 >=

instance P (Cmp 'CGe p q) x => P (p >= q) x where
  type PP (p >= q) x = PP (Cmp 'CGe p q) x
  eval _ = eval (Proxy @(Cmp 'CGe p q))

data p == q
infix 4 ==

instance P (Cmp 'CEq p q) x => P (p == q) x where
  type PP (p == q) x = PP (Cmp 'CEq p q) x
  eval _ = eval (Proxy @(Cmp 'CEq p q))

data p <= q
infix 4 <=

instance P (Cmp 'CLe p q) x => P (p <= q) x where
  type PP (p <= q) x = PP (Cmp 'CLe p q) x
  eval _ = eval (Proxy @(Cmp 'CLe p q))

data p < q
infix 4 <

instance P (Cmp 'CLt p q) x => P (p < q) x where
  type PP (p < q) x = PP (Cmp 'CLt p q) x
  eval _ = eval (Proxy @(Cmp 'CLt p q))

data p /= q
infix 4 /=

instance P (Cmp 'CNe p q) x => P (p /= q) x where
  type PP (p /= q) x = PP (Cmp 'CNe p q) x
  eval _ = eval (Proxy @(Cmp 'CNe p q))

--type p + q = Bin 'BAdd p q
--type p - q = Bin 'BSub p q
--type p * q = Bin 'BMult p q

--type p > q = Cmp 'CGt p q
--type p >= q = Cmp 'CGe p q
--type p == q = Cmp 'CEq p q
--type p /= q = Cmp 'CNe p q
--type p <= q = Cmp 'CLe p q
--type p < q = Cmp 'CLt p q

type Gt n = I > n
type Ge n = I >= n
type Same n = I == n
type Le n = I <= n
type Lt n = I < n
type Ne n = I /= n

--type p >~ q = CmpI 'CGt p q
--type p >=~ q = CmpI 'CGe p q
--type p ==~ q = CmpI 'CEq p q
--type p <=~ q = CmpI 'CLe p q
--type p <~ q = CmpI 'CLt p q
--type p /=~ q = CmpI 'CNe p q

data p >~ q
infix 4 >~

instance P (CmpI 'CGt p q) x => P (p >~ q) x where
  type PP (p >~ q) x = PP (CmpI 'CGt p q) x
  eval _ = eval (Proxy @(CmpI 'CGt p q))

data p >=~ q
infix 4 >=~

instance P (CmpI 'CGe p q) x => P (p >=~ q) x where
  type PP (p >=~ q) x = PP (CmpI 'CGe p q) x
  eval _ = eval (Proxy @(CmpI 'CGe p q))

data p ==~ q
infix 4 ==~

instance P (CmpI 'CEq p q) x => P (p ==~ q) x where
  type PP (p ==~ q) x = PP (CmpI 'CEq p q) x
  eval _ = eval (Proxy @(CmpI 'CEq p q))

data p <=~ q
infix 4 <=~

instance P (CmpI 'CLe p q) x => P (p <=~ q) x where
  type PP (p <=~ q) x = PP (CmpI 'CLe p q) x
  eval _ = eval (Proxy @(CmpI 'CLe p q))

data p <~ q
infix 4 <~

instance P (CmpI 'CLt p q) x => P (p <~ q) x where
  type PP (p <~ q) x = PP (CmpI 'CLt p q) x
  eval _ = eval (Proxy @(CmpI 'CLt p q))

data p /=~ q
infix 4 /=~

instance P (CmpI 'CNe p q) x => P (p /=~ q) x where
  type PP (p /=~ q) x = PP (CmpI 'CNe p q) x
  eval _ = eval (Proxy @(CmpI 'CNe p q))


class GetBinOp (k :: BinOp) where
  getBinOp :: (Num a, a ~ b) => (String, a -> b -> a)

instance GetBinOp 'BMult where
  getBinOp = ("*",(*))
instance GetBinOp 'BSub where
  getBinOp = ("-",(-))
instance GetBinOp 'BAdd where
  getBinOp = ("+",(+))

-- | addition, multiplication and subtraction
--
-- >>> pz @(Fst Id * Snd Id) (13,5)
-- Present 65
-- PresentT 65
--
-- >>> pz @(Fst Id + 4 * Length (Snd Id) - 4) (3,"hello")
-- Present 19
-- PresentT 19
--
data Bin (op :: BinOp) p q

instance (GetBinOp op
        , PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Num (PP p a)
        ) => P (Bin op p q) a where
  type PP (Bin op p q) a = PP p a
  eval _ opts a = do
    let (s,f) = getBinOp @op
    lr <- runPQ s (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p `f` q
        in mkNode opts (PresentT d) [show p <> " " <> s <> " " <> show q <> " = " <> show d] [hh pp, hh qq]

-- | fractional division
--
-- >>> pz @(Fst Id / Snd Id) (13,2)
-- Present 6.5
-- PresentT 6.5
--
-- >>> pz @(ToRational 13 / Id) 0
-- Error (/) zero denominator
-- FailT "(/) zero denominator"
--
-- >>> pz @(12 % 7 / 14 % 5 + Id) 12.4
-- Present 3188 % 245
-- PresentT (3188 % 245)
--
data p / q
infixl 7 /

instance (PP p a ~ PP q a
        , Eq (PP q a)
        , P p a
        , P q a
        , Show (PP p a)
        , Fractional (PP p a)
        ) => P (p / q) a where
  type PP (p / q) a = PP p a
  eval _ opts a = do
    let msg0 = "(/)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (FailT msg1) [msg1] [hh pp, hh qq]
         | otherwise ->
            let d = p / q
            in mkNode opts (PresentT d) [show p <> " / " <> show q <> " = " <> show d] [hh pp, hh qq]

-- | creates a 'Rational' value
--
-- >>> pz @(Id < 21 % 5) (-3.1)
-- True
-- TrueT
--
-- >>> pz @(Id < 21 % 5) 4.5
-- False
-- FalseT
--
-- >>> pz @(Fst Id % Snd Id) (13,2)
-- Present 13 % 2
-- PresentT (13 % 2)
--
-- >>> pz @(13 % Id) 0
-- Error MkRatio zero denominator
-- FailT "MkRatio zero denominator"
--
-- >>> pz @(4 % 3 + 5 % 7) "asfd"
-- Present 43 % 21
-- PresentT (43 % 21)
--
-- >>> pz @(4 -% 7 * 5 -% 3) "asfd"
-- Present 20 % 21
-- PresentT (20 % 21)
--
-- >>> pz @(Negate (14 % 3)) ()
-- Present (-14) % 3
-- PresentT ((-14) % 3)
--
-- >>> pz @(14 % 3) ()
-- Present 14 % 3
-- PresentT (14 % 3)
--
-- >>> pz @(Negate (14 % 3) ==! FromIntegral _ (Negate 5)) ()
-- Present GT
-- PresentT GT
--
-- >>> pz @(14 -% 3 ==! 5 -% 1) "aa"
-- Present GT
-- PresentT GT
--
-- >>> pz @(Negate (14 % 3) ==! Negate 5 % 2) ()
-- Present LT
-- PresentT LT
--
-- >>> pz @(14 -% 3 * 5 -% 1) ()
-- Present 70 % 3
-- PresentT (70 % 3)
--
-- >>> pz @(14 % 3 ==! 5 % 1) ()
-- Present LT
-- PresentT LT
--
-- >>> pz @(15 % 3 / 4 % 2) ()
-- Present 5 % 2
-- PresentT (5 % 2)
--
data p % q
infixl 8 %

data p -% q -- = Negate (p % q)
infixl 8 -%

instance P (Negate (p % q)) x => P (p -% q) x where
  type PP (p -% q) x = PP (Negate (p % q)) x
  eval _ = eval (Proxy @(Negate (p % q)))

instance (Integral (PP p x)
        , Integral (PP q x)
        , Eq (PP q x)
        , P p x
        , P q x
        , Show (PP p x)
        , Show (PP q x)
        ) => P (p % q) x where
  type PP (p % q) x = Rational
  eval _ opts x = do
    let msg0 = "MkRatio"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg0 <> " zero denominator"
                     in mkNode opts (FailT msg1) [msg1] [hh pp, hh qq]
         | otherwise ->
            let d = fromIntegral p % fromIntegral q
            in mkNode opts (PresentT d) [show p <> " % " <> show q <> " = " <> show d] [hh pp, hh qq]


-- | similar to 'negate'
--
-- >>> pz @(Negate Id) 14
-- Present -14
-- PresentT (-14)
--
-- >>> pz @(Negate (Fst Id * Snd Id)) (14,3)
-- Present -42
-- PresentT (-42)
--
-- >>> pz @(Negate (15 -% 4)) "abc"
-- Present 15 % 4
-- PresentT (15 % 4)
--
-- >>> pz @(Negate (15 % 3)) ()
-- Present (-5) % 1
-- PresentT ((-5) % 1)
--
-- >>> pz @(Negate (Fst Id % Snd Id)) (14,3)
-- Present (-14) % 3
-- PresentT ((-14) % 3)
--
data Negate p

instance (Show (PP p x), Num (PP p x), P p x) => P (Negate p) x where
  type PP (Negate p) x = PP p x
  eval _ opts x = do
    let msg0 = "Negate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = negate p
        in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]


-- | similar to 'abs'
--
-- >>> pz @(Abs Id) (-14)
-- Present 14
-- PresentT 14
--
-- >>> pz @(Abs (Snd Id)) ("xx",14)
-- Present 14
-- PresentT 14
--
-- >>> pz @(Abs Id) 0
-- Present 0
-- PresentT 0
--
-- >>> pz @(Abs (Negate 44)) "aaa"
-- Present 44
-- PresentT 44
--
data Abs p

instance (Show (PP p x), Num (PP p x), P p x) => P (Abs p) x where
  type PP (Abs p) x = PP p x
  eval _ opts x = do
    let msg0 = "Abs"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = abs p
        in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]



-- | similar to 'signum'
--
-- >>> pz @(Signum Id) (-14)
-- Present -1
-- PresentT (-1)
--
-- >>> pz @(Signum Id) 14
-- Present 1
-- PresentT 1
--
-- >>> pz @(Signum Id) 0
-- Present 0
-- PresentT 0
--
data Signum p

instance (Show (PP p x), Num (PP p x), P p x) => P (Signum p) x where
  type PP (Signum p) x = PP p x
  eval _ opts x = do
    let msg0 = "Signum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = signum p
        in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]

-- | unwraps a value (see '_Wrapped'')
--
-- >>> pz @(Unwrap Id) (SG.Sum (-13))
-- Present -13
-- PresentT (-13)
--
data Unwrap p

instance (PP p x ~ s
        , P p x
        , Show s
        , Show (Unwrapped s)
        , Wrapped s
        ) => P (Unwrap p) x where
  type PP (Unwrap p) x = Unwrapped (PP p x)
  eval _ opts x = do
    let msg0 = "Unwrap"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = p ^. _Wrapped'
        in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]

-- | wraps a value (see '_Wrapped'' and '_Unwrapped'')
--
-- >>> :m + Data.List.NonEmpty
-- >>> pz @(Wrap (SG.Sum _) Id) (-13)
-- Present Sum {getSum = -13}
-- PresentT (Sum {getSum = -13})
--
-- >>> pz @(Wrap SG.Any (Ge 4)) 13
-- Present Any {getAny = True}
-- PresentT (Any {getAny = True})
--
-- >>> pz @(Wrap (NonEmpty _) (Uncons >> 'Just Id)) "abcd"
-- Present 'a' :| "bcd"
-- PresentT ('a' :| "bcd")
--
data Wrap' t p

instance (Show (PP p x)
        , P p x
        , Unwrapped (PP s x) ~ PP p x
        , Wrapped (PP s x)
        , Show (PP s x)
        ) => P (Wrap' s p) x where
  type PP (Wrap' s p) x = PP s x
  eval _ opts x = do
    let msg0 = "Wrap"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = p ^. _Unwrapped'
        in mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]

data Wrap (t :: Type) p
type WrapT (t :: Type) p = Wrap' (Hole t) p

instance P (WrapT t p) x => P (Wrap t p) x where
  type PP (Wrap t p) x = PP (WrapT t p) x
  eval _ = eval (Proxy @(WrapT t p))
-- | similar to 'coerce'
--
-- >>> pz @(Coerce (SG.Sum Integer)) (Identity (-13))
-- Present Sum {getSum = -13}
-- PresentT (Sum {getSum = -13})
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d a] []

-- can coerce over a functor: but need to provide type of 'a' and 't' explicitly

-- | see 'Coerce': coerce over a functor
--
-- >>> pz @(Coerce2 (SG.Sum Integer)) [Identity (-13), Identity 4, Identity 99]
-- Present [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
-- PresentT [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
--
-- >>> pz @(Coerce2 (SG.Sum Integer)) (Just (Identity (-13)))
-- Present Just (Sum {getSum = -13})
-- PresentT (Just (Sum {getSum = -13}))
--
-- >>> pz @(Coerce2 (SG.Sum Int)) (Nothing @(Identity Int))
-- Present Nothing
-- PresentT Nothing
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d fa] []

-- | lift mempty over a Functor
--
-- >>> pz @(MEmpty2 (SG.Product Int)) [Identity (-13), Identity 4, Identity 99]
-- Present [Product {getProduct = 1},Product {getProduct = 1},Product {getProduct = 1}]
-- PresentT [Product {getProduct = 1},Product {getProduct = 1},Product {getProduct = 1}]
--
data MEmpty2' t

instance (Show (f a)
        , Show (f (PP t (f a)))
        , Functor f
        , Monoid (PP t (f a))
        ) => P (MEmpty2' t) (f a) where
  type PP (MEmpty2' t) (f a) = f (PP t (f a))
  eval _ opts fa =
    let msg0 = "MEmpty2"
        b = mempty <$> fa
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b fa] []

data MEmpty2 (t :: Type)
type MEmpty2T (t :: Type) = MEmpty2' (Hole t)

instance P (MEmpty2T t) x => P (MEmpty2 t) x where
  type PP (MEmpty2 t) x = PP (MEmpty2T t) x
  eval _ = eval (Proxy @(MEmpty2T t))

-- | lift pure over a Functor
--
-- >>> pz @(Pure2 (Either String)) [1,2,4]
-- Present [Right 1,Right 2,Right 4]
-- PresentT [Right 1,Right 2,Right 4]
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
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b fa] []

-- | similar to 'reverse'
--
-- >>> pz @Reverse [1,2,4]
-- Present [4,2,1]
-- PresentT [4,2,1]
--
-- >>> pz @Reverse "AbcDeF"
-- Present "FeDcbA"
-- PresentT "FeDcbA"
--
data Reverse

instance (Show a, as ~ [a]) => P Reverse as where
  type PP Reverse as = as
  eval _ opts as =
    let msg0 = "Reverse"
        d = reverse as
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d as] []

-- | reverses using 'reversing'
--
-- >>> pz @ReverseL (T.pack "AbcDeF")
-- Present "FeDcbA"
-- PresentT "FeDcbA"
--
-- >>> pz @ReverseL ("AbcDeF" :: String)
-- Present "FeDcbA"
-- PresentT "FeDcbA"
--
data ReverseL

instance (Show t, Reversing t) => P ReverseL t where
  type PP ReverseL t = t
  eval _ opts as =
    let msg0 = "ReverseL"
        d = as ^. reversed
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d as] []

-- | swaps using 'SwapC'
--
-- >>> pz @Swap (Left 123)
-- Present Right 123
-- PresentT (Right 123)
--
-- >>> pz @Swap (Right 123)
-- Present Left 123
-- PresentT (Left 123)
--
-- >>> pz @Swap (These 'x' 123)
-- Present These 123 'x'
-- PresentT (These 123 'x')
--
-- >>> pz @Swap (This 'x')
-- Present That 'x'
-- PresentT (That 'x')
--
-- >>> pz @Swap (That 123)
-- Present This 123
-- PresentT (This 123)
--
-- >>> pz @Swap (123,'x')
-- Present ('x',123)
-- PresentT ('x',123)
--
-- >>> pz @Swap (Left "abc")
-- Present Right "abc"
-- PresentT (Right "abc")
--
-- >>> pz @Swap (Right 123)
-- Present Left 123
-- PresentT (Left 123)
--
data Swap

class Bifunctor p => SwapC p where -- (p :: Type -> Type -> Type) where
  swapC :: p a b -> p b a
instance SwapC Either where
  swapC (Left a) = Right a
  swapC (Right a) = Left a
instance SwapC These where
  swapC (This a) = That a
  swapC (That b) = This b
  swapC (These a b) = These b a
instance SwapC (,) where
  swapC (a,b) = (b,a)

instance (Show (p a b)
        , SwapC p
        , Show (p b a)
        ) => P Swap (p a b) where
  type PP Swap (p a b) = p b a
  eval _ opts pab =
    let msg0 = "Swap"
        d = swapC pab
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d pab] []

-- | assoc using 'AssocC'
--
-- >>> pz @Assoc (This (These 123 'x'))
-- Present These 123 (This 'x')
-- PresentT (These 123 (This 'x'))
--
-- >>> pz @Assoc ((99,'a'),True)
-- Present (99,('a',True))
-- PresentT (99,('a',True))
--
-- >>> pz @Assoc ((99,'a'),True)
-- Present (99,('a',True))
-- PresentT (99,('a',True))
--
-- >>> pz @Assoc (Right "Abc" :: Either (Either () ()) String)
-- Present Right (Right "Abc")
-- PresentT (Right (Right "Abc"))
--
-- >>> pz @Assoc (Left (Left 'x'))
-- Present Left 'x'
-- PresentT (Left 'x')
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

-- copied from Data.These
partitionThese :: [These a b] -> ([a], [b], [(a, b)])
partitionThese [] = ([], [], [])
partitionThese (t:ts) = case t of
    This x    -> (x : xs,     ys,         xys)
    That y    -> (    xs, y : ys,         xys)
    These x y -> (    xs,     ys, (x,y) : xys)
  where
    ~(xs,ys,xys) = partitionThese ts

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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d pabc] []

-- | unassoc using 'AssocC'
--
-- >>> pz @Unassoc (These 123 (This 'x'))
-- Present This (These 123 'x')
-- PresentT (This (These 123 'x'))
--
-- >>> pz @Unassoc (99,('a',True))
-- Present ((99,'a'),True)
-- PresentT ((99,'a'),True)
--
-- >>> pz @Unassoc (This 10 :: These Int (These Bool ()))
-- Present This (This 10)
-- PresentT (This (This 10))
--
-- >>> pz @Unassoc (Right (Right 123))
-- Present Right 123
-- PresentT (Right 123)
--
-- >>> pz @Unassoc (Left 'x' :: Either Char (Either Bool Double))
-- Present Left (Left 'x')
-- PresentT (Left (Left 'x'))
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d pabc] []

-- | bounded 'succ' function
--
-- >>> pz @(SuccB' Id) (13 :: Int)
-- Present 14
-- PresentT 14
--
-- >>> pz @(SuccB' Id) LT
-- Present EQ
-- PresentT EQ
--
-- >>> pz @(SuccB 'LT Id) GT
-- Present LT
-- PresentT LT
--
-- >>> pz @(SuccB' Id) GT
-- Error Succ bounded
-- FailT "Succ bounded"
--
data SuccB p q

data SuccB' q
type SuccBT' q = SuccB (Failp "Succ bounded") q

instance P (SuccBT' q) x => P (SuccB' q) x where
  type PP (SuccB' q) x = PP (SuccBT' q) x
  eval _ = eval (Proxy @(SuccBT' q))


instance (PP q x ~ a
        , P q x
        , P p (Proxy a)
        , PP p (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (SuccB p q) x where
  type PP (SuccB p q) x = PP q x
  eval _ opts x = do
    let msg0 = "SuccB"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case succMay q of
          Nothing -> do
             let msg1 = msg0 <> " out of range"
             pp <- eval (Proxy @p) opts (Proxy @a)
             pure $ case getValueLR opts msg1 pp [hh qq] of
               Left e -> e
               Right _ -> mkNode opts (_tBool pp) [msg1] [hh qq, hh pp]
          Just n -> pure $ mkNode opts (PresentT n) [show01 opts msg0 n q] [hh qq]

-- | bounded 'pred' function
--
-- >>> pz @(PredB' Id) (13 :: Int)
-- Present 12
-- PresentT 12
--
-- >>> pz @(PredB' Id) LT
-- Error Pred bounded
-- FailT "Pred bounded"
--
data PredB p q

data PredB' q
type PredBT' q = PredB (Failp "Pred bounded") q

instance P (PredBT' q) x => P (PredB' q) x where
  type PP (PredB' q) x = PP (PredBT' q) x
  eval _ = eval (Proxy @(PredBT' q))


instance (PP q x ~ a
        , P q x
        , P p (Proxy a)
        , PP p (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (PredB p q) x where
  type PP (PredB p q) x = PP q x
  eval _ opts x = do
    let msg0 = "PredB"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case predMay q of
          Nothing -> do
             let msg1 = msg0 <> " out of range"
             pp <- eval (Proxy @p) opts (Proxy @a)
             pure $ case getValueLR opts msg1 pp [hh qq] of
               Left e -> e
               Right _ -> mkNode opts (_tBool pp) [msg1] [hh qq, hh pp]
          Just n -> pure $ mkNode opts (PresentT n) [show01 opts msg0 n q] [hh qq]


-- | unbounded 'succ' function
--
-- >>> pz @(Succ Id) 13
-- Present 14
-- PresentT 14
--
-- >>> pz @(Succ Id) LT
-- Present EQ
-- PresentT EQ
--
-- >>> pz @(Succ Id) GT
-- Error Succ IO e=Prelude.Enum.Ordering.succ: bad argument
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument"
--
data Succ p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (Succ p) x where
  type PP (Succ p) x = PP p x
  eval _ opts x = do
    let msg0 = "Succ"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (succ p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) [msg0 <> show0 opts " " p] [hh pp]
          Right n -> mkNode opts (PresentT n) [show01 opts msg0 n p] [hh pp]


-- | unbounded 'pred' function
--
-- >>> pz @(Pred Id) 13
-- Present 12
-- PresentT 12
--
-- >>> pz @(Pred Id) LT
-- Error Pred IO e=Prelude.Enum.Ordering.pred: bad argument
-- FailT "Pred IO e=Prelude.Enum.Ordering.pred: bad argument"
--

data Pred p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (Pred p) x where
  type PP (Pred p) x = PP p x
  eval _ opts x = do
    let msg0 = "Pred"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (pred p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) [msg0 <> show0 opts " " p] [hh pp]
          Right n -> mkNode opts (PresentT n) [show01 opts msg0 n p] [hh pp]


-- | 'fromEnum' function
--
-- >>> pz @(FromEnum Id) 'x'
-- Present 120
-- PresentT 120
--
data FromEnum p

instance (Show a
        , Enum a
        , PP p x ~ a
        , P p x
        ) => P (FromEnum p) x where
  type PP (FromEnum p) x = Int
  eval _ opts x = do
    let msg0 = "FromEnum"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let n = fromEnum p
        in mkNode opts (PresentT n) [show01 opts msg0 n p] [hh pp]

-- | unsafe 'toEnum' function
--
-- >>> pz @(ToEnum Char Id) 120
-- Present 'x'
-- PresentT 'x'
data ToEnum' t p

instance (PP p x ~ a
        , P p x
        , Show a
        , Enum (PP t x)
        , Show (PP t x)
        , Integral a
        ) => P (ToEnum' t p) x where
  type PP (ToEnum' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ToEnum"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        lr <- catchit @_ @E.SomeException (toEnum $! fromIntegral p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg0 <> " " <> e)) [msg0 <> show0 opts " " p] [hh pp]
          Right n -> mkNode opts (PresentT n) [show01 opts msg0 n p] [hh pp]

data ToEnum (t :: Type) p
type ToEnumT (t :: Type) p = ToEnum' (Hole t) p

instance P (ToEnumT t p) x => P (ToEnum t p) x where
  type PP (ToEnum t p) x = PP (ToEnumT t p) x
  eval _ = eval (Proxy @(ToEnumT t p))
-- | bounded 'toEnum' function
--
-- >>> pz @(ToEnumBDef Ordering LT) 2
-- Present GT
-- PresentT GT
--
-- >>> pz @(ToEnumBDef Ordering LT) 6
-- Present LT
-- PresentT LT
--
-- >>> pz @(ToEnumBFail Ordering) 6
-- Error ToEnum bounded
-- FailT "ToEnum bounded"
--
data ToEnumBDef' t def

instance (P def (Proxy (PP t a))
        , PP def (Proxy (PP t a)) ~ (PP t a)
        , Show a
        , Show (PP t a)
        , Bounded (PP t a)
        , Enum (PP t a)
        , Integral a
        ) => P (ToEnumBDef' t def) a where
  type PP (ToEnumBDef' t def) a = PP t a
  eval _ opts a = do
    let msg0 = "ToEnumBDef"
    case toEnumMay $ fromIntegral a of
      Nothing -> do
         let msg1 = msg0 <> " out of range"
         pp <- eval (Proxy @def) opts (Proxy @(PP t a))
         pure $ case getValueLR opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNode opts (_tBool pp) [msg1] [hh pp]
      Just n -> pure $ mkNode opts (PresentT n) [show01 opts msg0 n a] []

data ToEnumBDef (t :: Type) def
type ToEnumBDefT (t :: Type) def = ToEnumBDef' (Hole t) def

instance P (ToEnumBDefT t def) x => P (ToEnumBDef t def) x where
  type PP (ToEnumBDef t def) x = PP (ToEnumBDefT t def) x
  eval _ = eval (Proxy @(ToEnumBDefT t def))

data ToEnumBFail (t :: Type)
type ToEnumBFailT (t :: Type) = ToEnumBDef' (Hole t) (Failp "ToEnum bounded")

instance P (ToEnumBFailT t) x => P (ToEnumBFail t) x where
  type PP (ToEnumBFail t) x = PP (ToEnumBFailT t) x
  eval _ = eval (Proxy @(ToEnumBFailT t))

-- | a predicate on prime numbers
--
-- >>> pz @(Prime Id) 2
-- True
-- TrueT
--
-- >>> pz @(Map '(Id,Prime Id) Id) [0..12]
-- Present [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
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
        let b = isPrime $ fromIntegral p
        in mkNodeB opts b [msg0 <> show1 opts " | " p] []

isPrime :: Integer -> Bool
isPrime n = n==2 || n>2 && all ((> 0).rem n) (2:[3,5 .. floor . sqrt @Double . fromIntegral $ n+1])

-- empty lists at the type level wont work here

-- | filters a list \'q\' keeping or removing those elements in \'p\'
--
-- >>> pz @(Keep '[5] '[1,5,5,2,5,2]) ()
-- Present [5,5,5]
-- PresentT [5,5,5]
--
-- >>> pz @(Keep '[0,1,1,5] '[1,5,5,2,5,2]) ()
-- Present [1,5,5,5]
-- PresentT [1,5,5,5]
--
-- >>> pz @(Remove '[5] '[1,5,5,2,5,2]) ()
-- Present [1,2,2]
-- PresentT [1,2,2]
--
-- >>> pz @(Remove '[0,1,1,5] '[1,5,5,2,5,2]) ()
-- Present [2,2]
-- PresentT [2,2]
--
-- >>> pz @(Remove '[99] '[1,5,5,2,5,2]) ()
-- Present [1,5,5,2,5,2]
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove '[99,91] '[1,5,5,2,5,2]) ()
-- Present [1,5,5,2,5,2]
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove Id '[1,5,5,2,5,2]) []
-- Present [1,5,5,2,5,2]
-- PresentT [1,5,5,2,5,2]
--
-- >>> pz @(Remove '[] '[1,5,5,2,5,2]) 44 -- works if you make this a number!
-- Present [1,5,5,2,5,2]
-- PresentT [1,5,5,2,5,2]
--
data KeepImpl (keep :: Bool) p q

instance (GetBool keep
        , Eq a
        , Show a
        , P p x
        , P q x
        , PP p x ~ PP q x
        , PP q x ~ [a]
        ) => P (KeepImpl keep p q) x where
  type PP (KeepImpl keep p q) x = PP q x
  eval _ opts x = do
    let msg0 = if keep then "Keep" else "Remove"
        keep = getBool @keep
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let ret = filter (bool not id keep . (`elem` p)) q
        in mkNode opts (PresentT ret) [show01' opts msg0 ret "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]

data Keep p q
type KeepT p q = KeepImpl 'True p q

instance P (KeepT p q) x => P (Keep p q) x where
  type PP (Keep p q) x = PP (KeepT p q) x
  eval _ = eval (Proxy @(KeepT p q))

data Remove p q
type RemoveT p q = KeepImpl 'False p q

instance P (RemoveT p q) x => P (Remove p q) x where
  type PP (Remove p q) x = PP (RemoveT p q) x
  eval _ = eval (Proxy @(RemoveT p q))

-- | 'elem' function
--
-- >>> pz @(Elem (Fst Id) (Snd Id)) ('x',"abcdxy")
-- True
-- TrueT
--
-- >>> pz @(Elem (Fst Id) (Snd Id)) ('z',"abcdxy")
-- False
-- FalseT
--
data Elem p q

instance ([PP p a] ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Eq (PP p a)
         ) => P (Elem p q) a where
  type PP (Elem p q) a = Bool
  eval _ opts a = do
    let msg0 = "Elem"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `elem` q
        in mkNodeB opts b [show p <> " `elem` " <> show q] [hh pp, hh qq]

--type Head' p = HeadFail "Head(empty)" p
--type Tail' p = TailFail "Tail(empty)" p
--type Last p = LastFail "Last(empty)" p
--type Init' p = InitFail "Init(empty)" p

-- | similar to fmap fst
--
-- >>> pz @FMapFst (Just (13,"Asf"))
-- Present Just 13
-- PresentT (Just 13)
--
-- to make this work we grab the fst or snd out of the Maybe so it is a head or not/ is a tail or not etc!
-- we still have access to the whole original list so we dont lose anything!
data FMapFst

instance Functor f => P FMapFst (f (a,x)) where
  type PP FMapFst (f (a,x)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (fst <$> mb)) ["FMapFst"] []

-- | similar to fmap snd
--
-- >>> pz @FMapSnd (Just ("asf",13))
-- Present Just 13
-- PresentT (Just 13)
--
data FMapSnd

instance Functor f => P FMapSnd (f (x,a)) where
  type PP FMapSnd (f (x,a)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (snd <$> mb)) ["FMapSnd"] []

-- | takes the head or default of a list-like object
--
-- see 'ConsT' for other supported types eg 'Seq.Seq'
--
-- >>> pz @(HeadDef 444 Id) []
-- Present 444
-- PresentT 444
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- Present 1
-- PresentT 1
--
-- >>> pz @(HeadDef 444 Id) [1..5]
-- Present 1
-- PresentT 1
--
-- >>> pz @(HeadDef (Char1 "w") Id) (Seq.fromList "abcdef")
-- Present 'a'
-- PresentT 'a'
--
-- >>> pz @(HeadDef (Char1 "w") Id) Seq.empty
-- Present 'w'
-- PresentT 'w'
--
-- >>> :set -XFlexibleContexts
-- >>> pz @(HeadDef (MEmptyT _) Id) ([] :: [SG.Sum Int])
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
-- >>> pz @(HeadDef (MEmptyT String) '[ "abc","def","asdfadf" ]) ()
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) (Snd Id)) (123,[ "abc","def","asdfadf" ])
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(HeadDef (MEmptyT _) (Snd Id)) (123,[])
-- Present ()
-- PresentT ()
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
-- >>> pz @(HeadFail "dude" Id) [ "abc","def","asdfadf" ]
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(HeadFail "empty list" Id) []
-- Error empty list
-- FailT "empty list"
--
data HeadFail msg q
type HeadFailT msg q = JustFail msg (q >> Uncons >> FMapFst)

instance P (HeadFailT msg q) x => P (HeadFail msg q) x where
  type PP (HeadFail msg q) x = PP (HeadFailT msg q) x
  eval _ = eval (Proxy @(HeadFailT msg q))

data TailDef p q
type TailDefT p q = JustDef p (q >> Uncons >> FMapSnd)

instance P (TailDefT p q) x => P (TailDef p q) x where
  type PP (TailDef p q) x = PP (TailDefT p q) x
  eval _ = eval (Proxy @(TailDefT p q))


data TailFail msg q
type TailFailT msg q = JustFail msg (q >> Uncons >> FMapSnd)

instance P (TailFailT msg q) x => P (TailFail msg q) x where
  type PP (TailFail msg q) x = PP (TailFailT msg q) x
  eval _ = eval (Proxy @(TailFailT msg q))


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

data LookupDef' v w p q
type LookupDefT' v w p q = JustDef p (q >> Lookup v w)

instance P (LookupDefT' v w p q) x => P (LookupDef' v w p q) x where
  type PP (LookupDef' v w p q) x = PP (LookupDefT' v w p q) x
  eval _ = eval (Proxy @(LookupDefT' v w p q))

data LookupFail' msg v w q
type LookupFailT' msg v w q = JustFail msg (q >> Lookup v w)

instance P (LookupFailT' msg v w q) x => P (LookupFail' msg v w q) x where
  type PP (LookupFail' msg v w q) x = PP (LookupFailT' msg v w q) x
  eval _ = eval (Proxy @(LookupFailT' msg v w q))

data LookupDef v w p
type LookupDefT v w p = LookupDef' v w p I

instance P (LookupDefT v w p) x => P (LookupDef v w p) x where
  type PP (LookupDef v w p) x = PP (LookupDefT v w p) x
  eval _ = eval (Proxy @(LookupDefT v w p))

data LookupFail msg v w
type LookupFailT msg v w = LookupFail' msg v w I

instance P (LookupFailT msg v w) x => P (LookupFail msg v w) x where
  type PP (LookupFail msg v w) x = PP (LookupFailT msg v w) x
  eval _ = eval (Proxy @(LookupFailT msg v w))

--type Just'  p = JustFail  "expected Just" p
data Left' p
type LeftT' p = LeftFail "expected Left"  p

instance P (LeftT' p) x => P (Left' p) x where
  type PP (Left' p) x = PP (LeftT' p) x
  eval _ = eval (Proxy @(LeftT' p))

data Right' p
type RightT' p = RightFail "expected Right" p

instance P (RightT' p) x => P (Right' p) x where
  type PP (Right' p) x = PP (RightT' p) x
  eval _ = eval (Proxy @(RightT' p))

data This'  p
type ThisT'  p = ThisFail  "expected This"  p

instance P (ThisT' p) x => P (This' p) x where
  type PP (This' p) x = PP (ThisT' p) x
  eval _ = eval (Proxy @(ThisT' p))

data That'  p
type ThatT'  p = ThatFail  "expected That"  p

instance P (ThatT' p) x => P (That' p) x where
  type PP (That' p) x = PP (ThatT' p) x
  eval _ = eval (Proxy @(ThatT' p))

data These' p
type TheseT' p = TheseFail "expected These" p

instance P (TheseT' p) x => P (These' p) x where
  type PP (These' p) x = PP (TheseT' p) x
  eval _ = eval (Proxy @(TheseT' p))


-- | similar to 'Control.Arrow.|||' but additionally gives \'p\' and \'q\' the original input
--
-- >>> pz @(EitherX (ShowP (Fst (Fst Id) + Snd Id)) (ShowP Id) (Snd Id)) (9,Left 123)
-- Present "132"
-- PresentT "132"
--
-- >>> pz @(EitherX (ShowP (Fst (Fst Id) + Snd Id)) (ShowP Id) (Snd Id)) (9,Right 'x')
-- Present "((9,Right 'x'),'x')"
-- PresentT "((9,Right 'x'),'x')"
--
-- >>> pz @(EitherX (ShowP Id) (ShowP (Second (Succ Id))) (Snd Id)) (9,Right 'x')
-- Present "((9,Right 'x'),'y')"
-- PresentT "((9,Right 'x'),'y')"
--
data EitherX p q r
instance (P r x
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
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right (Left a) -> do
        let msg1 = msg0 <> "(Left)"
        pp <- eval (Proxy @p) opts (x,a)
        pure $ case getValueLR opts msg1 pp [hh rr] of
          Left e -> e
          Right _ -> mkNode opts (_tBool pp) [msg1] [hh rr, hh pp]
      Right (Right b) -> do
        let msg1 = msg0 <> "(Right)"
        qq <- eval (Proxy @q) opts (x,b)
        pure $ case getValueLR opts msg1 qq [hh rr] of
          Left e -> e
          Right _ -> mkNode opts (_tBool qq) [msg1] [hh rr, hh qq]

type family EitherXT lr x p where
  EitherXT (Either a b) x p = PP p (x,a)
  EitherXT o _ _ = GL.TypeError (
      'GL.Text "EitherXT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | similar to 'Data.These.mergeTheseWith' but additionally provides \'p\', '\q'\ and \'r\' the original input as the first element in the tuple
--
-- >>> pz @(TheseX ((Fst (Fst Id) + Snd Id) >> ShowP Id) (ShowP Id) (Snd (Snd Id)) (Snd Id)) (9,This 123)
-- Present "132"
-- PresentT "132"
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (This 123)
-- Present (123,"fromthis")
-- PresentT (123,"fromthis")
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (That "fromthat")
-- Present (-99,"fromthat")
-- PresentT (-99,"fromthat")
--
-- >>> pz @(TheseX '(Snd Id,"fromthis") '(Negate 99,Snd Id) (Snd Id) Id) (These 123 "fromthese")
-- Present (123,"fromthese")
-- PresentT (123,"fromthese")
--
data TheseX p q r s

instance (P s x
        , P p (x,a)
        , P q (x,b)
        , P r (x,(a,b))
        , PP s x ~ These a b
        , PP p (x,a) ~ c
        , PP q (x,b) ~ c
        , PP r (x,(a,b)) ~ c
        ) => P (TheseX p q r s) x where
  type PP (TheseX p q r s) x = TheseXT (PP s x) x p
  eval _ opts x = do
    let msg0 = "TheseX"
    ss <- eval (Proxy @s) opts x
    case getValueLR opts msg0 ss [] of
      Left e -> pure e
      Right (This a) -> do
        let msg1 = msg0 <> "(This)"
        pp <- eval (Proxy @p) opts (x,a)
        pure $ case getValueLR opts msg1 pp [hh ss] of
          Left e -> e
          Right _ -> mkNode opts (_tBool pp) [msg1] [hh ss, hh pp]
      Right (That b) -> do
        let msg1 = msg0 <> "(That)"
        qq <- eval (Proxy @q) opts (x,b)
        pure $ case getValueLR opts msg1 qq [hh ss] of
          Left e -> e
          Right _ -> mkNode opts (_tBool qq) [msg1] [hh ss, hh qq]
      Right (These a b) -> do
        let msg1 = msg0 <> "(These)"
        rr <- eval (Proxy @r) opts (x,(a,b))
        pure $ case getValueLR opts msg1 rr [hh ss] of
          Left e -> e
          Right _ -> mkNode opts (_tBool rr) [msg1] [hh ss, hh rr]

type family TheseXT lr x p where
  TheseXT (These a b) x p = PP p (x,a)

-- | similar to 'maybe'
--
-- provides a Proxy to the result of \'q\' but does not provide the surrounding context
--
-- >>> pz @(MaybeIn "foundnothing" (ShowP (Pred Id))) (Just 20)
-- Present "19"
-- PresentT "19"
--
-- >>> pz @(MaybeIn "found nothing" (ShowP (Pred Id))) Nothing
-- Present "found nothing"
-- PresentT "found nothing"
--
data MaybeIn p q

-- tricky: the nothing case is the proxy of PP q a: ie proxy of the final result
instance (P q a
        , Show a
        , Show (PP q a)
        , PP p (Proxy (PP q a)) ~ PP q a
        , P p (Proxy (PP q a))
        ) => P (MaybeIn p q) (Maybe a) where
  type PP (MaybeIn p q) (Maybe a) = PP q a
  eval _ opts ma = do
    let msg0 = "MaybeIn"
    case ma of
      Nothing -> do
        let msg1 = msg0 <> "(Nothing)"
        pp <- eval (Proxy @p) opts (Proxy @(PP q a))
        pure $ case getValueLR opts msg1 pp [] of
          Left e -> e
          Right b -> mkNode opts (_tBool pp) [msg1 <> show0 opts " " b <> " | Proxy"] [hh pp]
      Just a -> do
        let msg1 = msg0 <> "(Nothing)"
        qq <- eval (Proxy @q) opts a
        pure $ case getValueLR opts msg1 qq [] of
          Left e -> e
          Right b -> mkNode opts (_tBool qq) [show01 opts msg1 b a] [hh qq]

data IsNothing
type IsNothingT = MaybeIn 'True 'False

instance P IsNothingT x => P IsNothing x where
  type PP IsNothing x = PP IsNothingT x
  eval _ = eval (Proxy @IsNothingT)

data IsJust
type IsJustT = MaybeIn 'False 'True

instance P IsJustT x => P IsJust x where
  type PP IsJust x = PP IsJustT x
  eval _ = eval (Proxy @IsJustT)

data MapMaybe p q
type MapMaybeT p q = ConcatMap (p >> MaybeIn MEmptyP '[Id]) q

instance P (MapMaybeT p q) x => P (MapMaybe p q) x where
  type PP (MapMaybe p q) x = PP (MapMaybeT p q) x
  eval _ = eval (Proxy @(MapMaybeT p q))

-- | similar to 'Data.Either.catMaybes'
--
-- >>> pl @(CatMaybes Id) [Just 'a',Nothing,Just 'c',Just 'd',Nothing]
-- Present "acd" (Concat "acd" | ["a","","c","d",""])
-- PresentT "acd"
--
data CatMaybes q
type CatMaybesT q = MapMaybe Id q

instance P (CatMaybesT q) x => P (CatMaybes q) x where
  type PP (CatMaybes q) x = PP (CatMaybesT q) x
  eval _ = eval (Proxy @(CatMaybesT q))

-- | similar to 'SG.stimes'
--
-- >>> pz @(STimes 4 Id) (SG.Sum 3)
-- Present Sum {getSum = 12}
-- PresentT (Sum {getSum = 12})
--
-- >>> pz @(STimes 4 Id) "ab"
-- Present "abababab"
-- PresentT "abababab"
--
data STimes n p
instance (P n a
        , Integral (PP n a)
        , Semigroup (PP p a)
        , P p a
        , Show (PP p a)
        ) => P (STimes n p) a where
  type PP (STimes n p) a = PP p a
  eval _ opts a = do
    let msg0 = "STimes"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> (n::Int),p,pp,qq) ->
        let msg1 = msg0 <> show0 opts " " n <> " p=" <> show p
            b = SG.stimes n p
            in mkNode opts (PresentT b) [show01' opts msg1 b "n=" n <> show1 opts " | " p] [hh pp, hh qq]


-- | similar to 'pure'
--
-- >>> pz @(Pure Maybe Id) 4
-- Present Just 4
-- PresentT (Just 4)
--
-- >>> pz @(Pure [] Id) 4
-- Present [4]
-- PresentT [4]
--
-- >>> pz @(Pure (Either String) (Fst Id)) (13,True)
-- Present Right 13
-- PresentT (Right 13)
--
data Pure (t :: Type -> Type) p
instance (P p x
        , Show (PP p x)
        , Show (t (PP p x))
        , Applicative t
        ) => P (Pure t p) x where
  type PP (Pure t p) x = t (PP p x)
  eval _ opts x = do
    let msg0 = "Pure"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right a ->
        let b = pure a
        in mkNode opts (PresentT b) [show01 opts msg0 b a] [hh pp]

-- type PMEmpty = MEmptyT' 'Proxy  -- lifts 'a' to 'Proxy a' then we can use it with MEmptyP

-- | similar to 'mempty'
--
-- >>> pz @(MEmptyT (SG.Sum Int)) ()
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
-- no Monoid for Maybe a unless a is also a monoid but can use empty!
data MEmptyT' t
instance (Show (PP t a), Monoid (PP t a)) => P (MEmptyT' t) a where
  type PP (MEmptyT' t) a = PP t a
  eval _ opts _ =
    let msg0 = "MEmptyT"
        b = mempty @(PP t a)
    in pure $ mkNode opts (PresentT b) [msg0 <> show0 opts " " b] []

data MEmptyT (t :: Type)
type MEmptyTT (t :: Type) = MEmptyT' (Hole t)

instance P (MEmptyTT t) x => P (MEmptyT t) x where
  type PP (MEmptyT t) x = PP (MEmptyTT t) x
  eval _ = eval (Proxy @(MEmptyTT t))

data MEmptyP
type MEmptyPT = MEmptyT' Unproxy -- expects a proxy: so only some things work with this: eg MaybeIn

instance P MEmptyPT x => P MEmptyP x where
  type PP MEmptyP x = PP MEmptyPT x
  eval _ = eval (Proxy @MEmptyPT)

-- | similar to 'empty'
--
-- >>> pz @(EmptyT Maybe Id) ()
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @(EmptyT [] Id) ()
-- Present []
-- PresentT []
--
-- >>> pz @(EmptyT [] (Char1 "x")) (13,True)
-- Present ""
-- PresentT ""
--
-- >>> pz @(EmptyT (Either String) (Fst Id)) (13,True)
-- Present Left ""
-- PresentT (Left "")
--
data EmptyT (t :: Type -> Type) p

instance (P p x
        , PP p x ~ a
        , Show (t a)
        , Show a
        , Alternative t
        ) => P (EmptyT t p) x where
  type PP (EmptyT t p) x = t (PP p x)
  eval _ opts x = do
    let msg0 = "EmptyT"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = empty @t
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

data MkNothing' t -- works always! MaybeBool is a good alternative and then dont need the extra 't'

-- for this to be useful has to have 't' else we end up with tons of problems
instance P (MkNothing' t) a where
  type PP (MkNothing' t) a = Maybe (PP t a)
  eval _ opts _ =
    let msg0 = "MkNothing"
    in pure $ mkNode opts (PresentT Nothing) [msg0] []

data MkNothing (t :: Type)
type MkNothingT (t :: Type) = MkNothing' (Hole t)

instance P (MkNothing t) x where
  type PP (MkNothing t) x = PP (MkNothingT t) x
  eval _ = eval (Proxy @(MkNothingT t))

-- | 'GHC.Maybe.Just' constructor
--
-- >>> pz @(MkJust Id) 44
-- Present Just 44
-- PresentT (Just 44)
--
data MkJust p
instance (PP p x ~ a, P p x, Show a) => P (MkJust p) x where
  type PP (MkJust p) x = Maybe (PP p x)
  eval _ opts x = do
    let msg0 = "MkJust"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Just p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " Just " p] [hh pp]

-- | 'Data.Either.Left' constructor
--
-- >>> pz @(MkLeft _ Id) 44
-- Present Left 44
-- PresentT (Left 44)
--
data MkLeft' t p

instance (Show (PP p x), P p x) => P (MkLeft' t p) x where
  type PP (MkLeft' t p) x = Either (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkLeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Left p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " Left " p] [hh pp]

data MkLeft (t :: Type) p
type MkLeftT (t :: Type) p = MkLeft' (Hole t) p

instance P (MkLeftT t p) x => P (MkLeft t p) x where
  type PP (MkLeft t p) x = PP (MkLeftT t p) x
  eval _ = eval (Proxy @(MkLeftT t p))

-- | 'Data.Either.Right' constructor
--
-- >>> pz @(MkRight _ Id) 44
-- Present Right 44
-- PresentT (Right 44)
--
data MkRight' t p

instance (Show (PP p x), P p x) => P (MkRight' t p) x where
  type PP (MkRight' t p) x = Either (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkRight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Right p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " Right " p] [hh pp]

data MkRight (t :: Type) p
type MkRightT (t :: Type) p = MkRight' (Hole t) p

instance P (MkRightT t p) x => P (MkRight t p) x where
  type PP (MkRight t p) x = PP (MkRightT t p) x
  eval _ = eval (Proxy @(MkRightT t p))

-- | 'Data.These.This' constructor
--
-- >>> pz @(MkThis _ Id) 44
-- Present This 44
-- PresentT (This 44)
--
-- >>> pz @(Proxy Int >> MkThis' Unproxy 10) []
-- Present This 10
-- PresentT (This 10)
--
data MkThis' t p

instance (Show (PP p x), P p x) => P (MkThis' t p) x where
  type PP (MkThis' t p) x = These (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkThis"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = This p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " This " p] [hh pp]

data MkThis (t :: Type) p
type MkThisT (t :: Type) p = MkThis' (Hole t) p

instance P (MkThisT t p) x => P (MkThis t p) x where
  type PP (MkThis t p) x = PP (MkThisT t p) x
  eval _ = eval (Proxy @(MkThisT t p))

-- | 'Data.These.That' constructor
--
-- >>> pz @(MkThat _ Id) 44
-- Present That 44
-- PresentT (That 44)
--
data MkThat' t p

instance (Show (PP p x), P p x) => P (MkThat' t p) x where
  type PP (MkThat' t p) x = These (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkThat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = That p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " That " p] [hh pp]

data MkThat (t :: Type) p
type MkThatT (t :: Type) p = MkThat' (Hole t) p

instance P (MkThatT t p) x => P (MkThat t p) x where
  type PP (MkThat t p) x = PP (MkThatT t p) x
  eval _ = eval (Proxy @(MkThatT t p))

-- type MkThat t p = MkThis t p >> Swap
-- type MkThat' (t :: Type) = Pure (These t) Id -- t has to be a semigroup

-- | 'Data.These.These' constructor
--
-- >>> pz @(MkThese (Fst Id) (Snd Id)) (44,'x')
-- Present These 44 'x'
-- PresentT (These 44 'x')
--
data MkThese p q
instance (P p a
        , P q a
        , Show (PP p a)
        , Show (PP q a)
        ) => P (MkThese p q) a where
  type PP (MkThese p q) a = These (PP p a) (PP q a)
  eval _ opts a = do
    let msg0 = "MkThese"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = These p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

-- | similar to 'mconcat'
--
-- >>> pz @(MConcat Id) [SG.Sum 44, SG.Sum 12, SG.Sum 3]
-- Present Sum {getSum = 59}
-- PresentT (Sum {getSum = 59})
--
data MConcat p

instance (PP p x ~ [a]
        , P p x
        , Show a
        , Monoid a
        ) => P (MConcat p) x where
  type PP (MConcat p) x = ExtractAFromList (PP p x)
  eval _ opts x = do
    let msg0 = "MConcat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = mconcat p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

-- | similar to a limited form of 'foldMap'
--
-- >>> pz @(FoldMap (SG.Sum _) Id) [44, 12, 3]
-- Present 59
-- PresentT 59
--
-- >>> pz @(FoldMap (SG.Product _) Id) [44, 12, 3]
-- Present 1584
-- PresentT 1584
--
-- >>> type Ands' p = FoldMap SG.All p
-- >>> pz @(Ands' Id) [True,False,True,True]
-- Present False
-- PresentT False
--
-- >>> pz @(Ands' Id) [True,True,True]
-- Present True
-- PresentT True
--
-- >>> pz @(Ands' Id) []
-- Present True
-- PresentT True
--
-- >>> type Ors' p = FoldMap SG.Any p
-- >>> pz @(Ors' Id) [False,False,False]
-- Present False
-- PresentT False
--
-- >>> pz @(Ors' Id) []
-- Present False
-- PresentT False
--
-- >>> pz @(Ors' Id) [False,False,False,True]
-- Present True
-- PresentT True
--
-- >>> type AllPositive' = FoldMap SG.All (Map Positive Id)
-- >>> pz @AllPositive' [3,1,-5,10,2,3]
-- Present False
-- PresentT False
--
-- >>> type AllNegative' = FoldMap SG.All (Map Negative Id)
-- >>> pz @AllNegative' [-1,-5,-10,-2,-3]
-- Present True
-- PresentT True
--
-- >>> :set -XKindSignatures
-- >>> type Max' (t :: Type) = FoldMap (SG.Max t) Id -- requires t be Bounded for monoid instance
-- >>> pz @(Max' Int) [10,4,5,12,3,4]
-- Present 12
-- PresentT 12
--
data FoldMap (t :: Type) p
type FoldMapT (t :: Type) p = Map (Wrap t Id) p >> Unwrap (MConcat Id)

instance P (FoldMapT t p) x => P (FoldMap t p) x where
  type PP (FoldMap t p) x = PP (FoldMapT t p) x
  eval _ = eval (Proxy @(FoldMapT t p))

-- | similar to 'concat'
--
-- >>> pz @(Concat Id) ["abc","D","eF","","G"]
-- Present "abcDeFG"
-- PresentT "abcDeFG"
--
-- >>> pz @(Concat (Snd Id)) ('x',["abc","D","eF","","G"])
-- Present "abcDeFG"
-- PresentT "abcDeFG"
--
data Concat p

instance (Show a
        , Show (t [a])
        , PP p x ~ (t [a])
        , P p x
        , Foldable t
        ) => P (Concat p) x where
  type PP (Concat p) x = ExtractAFromTA (PP p x)
  eval _ opts x = do
    let msg0 = "Concat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = concat p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] [hh pp]

-- | similar to 'cycle' but for a fixed number \'n\'
--
-- >>> pz @(Cycle 5 Id) [1,2]
-- Present [1,2,1,2,1]
-- PresentT [1,2,1,2,1]
--
data Cycle n p

instance (Show a
        , Show (t a)
        , PP p x ~ (t a)
        , P p x
        , Integral (PP n x)
        , P n x
        , Foldable t
        ) => P (Cycle n p) x where
  type PP (Cycle n p) x = [ExtractAFromTA (PP p x)]
  eval _ opts x = do
    let msg0 = "Cycle"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts x []
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> n,p,nn,pp) ->
        let hhs = [hh nn, hh pp]
        in case chkSize opts msg0 p hhs of
            Left e ->  e
            Right () ->
              let msg1 = msg0 <> "("<> show n <> ")"
                  d = take n (cycle (toList p))
              in mkNode opts (PresentT d) [show01 opts msg1 d p] hhs

data ProxyT' t

instance Typeable t => P (ProxyT' (t :: Type)) a where
  type PP (ProxyT' t) a = Proxy (PP t a)
  eval _ opts _ =
    let t = showT @t
    in pure $ mkNode opts (PresentT Proxy) ["ProxyT(" <> show t ++ ")"] []

data ProxyT (t :: Type)
type ProxyTT (t :: Type) = ProxyT' (Hole t)

instance P (ProxyTT t) x => P (ProxyT t) x where
  type PP (ProxyT t) x = PP (ProxyTT t) x
  eval _ = eval (Proxy @(ProxyTT t))

-- | similar to 'Data.List.!!'
--
-- >>> pz @(Ix 4 "not found") ["abc","D","eF","","G"]
-- Present "G"
-- PresentT "G"
--
-- >>> pz @(Ix 40 "not found") ["abc","D","eF","","G"]
-- Present "not found"
-- PresentT "not found"
--
data Ix (n :: Nat) def

instance (P def (Proxy a)
        , PP def (Proxy a) ~ a
        , KnownNat n
        , Show a
        ) => P (Ix n def) [a] where
  type PP (Ix n def) [a] = a
  eval _ opts as = do
    let n = nat @n
        msg0 = "Ix " <> show n
    case as ^? ix n of
         Nothing -> do
           let msg1 = msg0 <> " not found"
           pp <- eval (Proxy @def) opts (Proxy @a)
           pure $ case getValueLR opts msg1 pp [] of
             Left e -> e
             Right _ -> mkNode opts (_tBool pp) [msg1] [hh pp]
         Just a -> pure $ mkNode opts (PresentT a) [msg0 <> show0 opts " " a] []

data Ix' (n :: Nat)
type IxT' (n :: Nat) = Ix n (Failp "Ix index not found")

instance P (IxT' n) x => P (Ix' n) x where
  type PP (Ix' n) x = PP (IxT' n) x
  eval _ = eval (Proxy @(IxT' n))

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
-- >>> pz @(IxL Id 2 "notfound") ["abc","D","eF","","G"]
-- Present "eF"
-- PresentT "eF"
--
-- >>> pz @(IxL Id 20 "notfound") ["abc","D","eF","","G"]
-- Present "notfound"
-- PresentT "notfound"
--
data IxL p q def -- p is the big value and q is the index and def is the default

instance (P q a
        , P p a
        , Show (PP p a)
        , Ixed (PP p a)
        , PP q a ~ Index (PP p a)
        , Show (Index (PP p a))
        , Show (IxValue (PP p a))
        , P r (Proxy (IxValue (PP p a)))
        , PP r (Proxy (IxValue (PP p a))) ~ IxValue (PP p a)
        )
   => P (IxL p q r) a where
  type PP (IxL p q r) a = IxValue (PP p a)
  eval _ opts a = do
    let msg0 = "IxL"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> show q <> ")"
        in case p ^? ix q of
             Nothing -> do
                rr <- eval (Proxy @r) opts (Proxy @(IxValue (PP p a)))
                pure $ case getValueLR opts msg1 rr [hh pp, hh qq] of
                  Left e -> e
                  Right _ -> mkNode opts (_tBool rr) [msg1 <> " index not found"] [hh pp, hh qq]
             Just ret -> pure $ mkNode opts (PresentT ret) [show01' opts msg1 ret "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
-- >>> pz @(Id !! 2) ["abc","D","eF","","G"]
-- Present "eF"
-- PresentT "eF"
--
-- >>> pz @(Id !! 20) ["abc","D","eF","","G"]
-- Error (!!) index not found
-- FailT "(!!) index not found"
--
-- >>> import qualified Data.Map.Strict as M
-- >>> pz @(Id !! "eF") (M.fromList (flip zip [0..] ["abc","D","eF","","G"]))
-- Present 2
-- PresentT 2
--
data p !! q
type BangBangT p q = IxL p q (Failp "(!!) index not found")

instance P (BangBangT p q) a => P (p !! q) a where
  type PP (p !! q) a = PP (BangBangT p q) a
  eval _ = eval (Proxy @(BangBangT p q))

-- | 'lookup' leveraging 'Ixed'
--
-- >>> pz @(Lookup Id 2) ["abc","D","eF","","G"]
-- Present Just "eF"
-- PresentT (Just "eF")
--
-- >>> pz @(Lookup Id 20) ["abc","D","eF","","G"]
-- Present Nothing
-- PresentT Nothing
--
data Lookup p q

instance (P q a
        , P p a
        , Show (PP p a)
        , Ixed (PP p a)
        , PP q a ~ Index (PP p a)
        , Show (Index (PP p a))
        , Show (IxValue (PP p a))
        )
   => P (Lookup p q) a where
  type PP (Lookup p q) a = Maybe (IxValue (PP p a))
  eval _ opts a = do
    let msg0 = "Lookup"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> show q <> ")"
            hhs = [hh pp, hh qq]
        in case p ^? ix q of
             Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " not found"] hhs
             Just ret -> mkNode opts (PresentT (Just ret)) [show01' opts msg1 ret "p=" p <> show1 opts " | q=" q] hhs

-- | 'Data.List.ands'
--
-- >>> pz @(Ands Id) [True,True,True]
-- True
-- TrueT
--
-- >>> pl @(Ands Id) [True,True,True,False]
-- False (Ands(4) i=3 | [True,True,True,False])
-- FalseT
--
-- >>> pz @(Ands Id) []
-- True
-- TrueT
--
data Ands p

instance (PP p x ~ t a
        , P p x
        , Show (t a)
        , Foldable t
        , a ~ Bool
        ) => P (Ands p) x where
  type PP (Ands p) x = Bool
  eval _ opts x = do
    let msg0 = "Ands"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let msg1 = msg0 ++ "(" ++ show (length p) ++ ")"
            w = case findIndex not (toList p) of
                  Nothing -> ""
                  Just i -> " i="++show i
        in mkNodeB opts (and p) [msg1 <> w <> show1 opts " | " p] [hh pp]

-- | 'Data.List.ors'
--
-- >>> pz @(Ors Id) [False,False,False]
-- False
-- FalseT
--
-- >>> pl @(Ors Id) [True,True,True,False]
-- True (Ors(4) i=0 | [True,True,True,False])
-- TrueT
--
-- >>> pl @(Ors Id) []
-- False (Ors(0) | [])
-- FalseT
--
data Ors p

instance (PP p x ~ t a
        , P p x
        , Show (t a)
        , Foldable t
        , a ~ Bool
        ) => P (Ors p) x where
  type PP (Ors p) x = Bool
  eval _ opts x = do
    let msg0 = "Ors"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let msg1 = msg0 ++ "(" ++ show (length p) ++ ")"
            w = case findIndex id (toList p) of
                  Nothing -> ""
                  Just i -> " i="++show i
        in mkNodeB opts (or p) [msg1 <> w <> show1 opts " | " p] [hh pp]

-- cant directly create a singleton type using '[] since the type of '[] is unknown. instead use 'Singleton' or 'EmptyT'

-- | similar to cons
--
-- >>> pz @(Fst Id :+ Snd Id) (99,[1,2,3,4])
-- Present [99,1,2,3,4]
-- PresentT [99,1,2,3,4]
--
-- >>> pz @(Snd Id :+ Fst Id) ([],5)
-- Present [5]
-- PresentT [5]
--
-- >>> pz @(123 :+ EmptyList _) "somestuff"
-- Present [123]
-- PresentT [123]
--
data p :+ q
infixr 5 :+

instance (P p x
        , P q x
        , Show (PP p x)
        , Show (PP q x)
        , Cons (PP q x) (PP q x) (PP p x) (PP p x)
        ) => P (p :+ q) x where
  type PP (p :+ q) x = PP q x
  eval _ opts z = do
    let msg0 = "(:+)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `cons` q
        in mkNode opts (PresentT b) [show01' opts msg0 b "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]

-- | similar to snoc
--
-- >>> pz @(Snd Id +: Fst Id) (99,[1,2,3,4])
-- Present [1,2,3,4,99]
-- PresentT [1,2,3,4,99]
--
-- >>> pz @(Fst Id +: Snd Id) ([],5)
-- Present [5]
-- PresentT [5]
--
-- >>> pz @(EmptyT [] Id +: 5) 5
-- Present [5]
-- PresentT [5]
--
data p +: q
infixl 5 +:

instance (P p x
        , P q x
        , Show (PP q x)
        , Show (PP p x)
        , Snoc (PP p x) (PP p x) (PP q x) (PP q x)
        ) => P (p +: q) x where
  type PP (p +: q) x = PP p x
  eval _ opts z = do
    let msg0 = "(+:)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `snoc` q
        in mkNode opts (PresentT b) [show01' opts msg0 b "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]

-- | 'Control.Lens.uncons'
--
-- >>> pz @Uncons [1,2,3,4]
-- Present Just (1,[2,3,4])
-- PresentT (Just (1,[2,3,4]))
--
-- >>> pz @Uncons []
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @Uncons (Seq.fromList "abc")
-- Present Just ('a',fromList "bc")
-- PresentT (Just ('a',fromList "bc"))
--
-- >>> pz @Uncons ("xyz" :: T.Text)
-- Present Just ('x',"yz")
-- PresentT (Just ('x',"yz"))
--
data Uncons

instance (Show (ConsT s)
        , Show s
        , Cons s s (ConsT s) (ConsT s)
        ) => P Uncons s where
  type PP Uncons s = Maybe (ConsT s,s)
  eval _ opts as =
    let msg0 = "Uncons"
        b = as ^? _Cons
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b as] []

-- | 'Control.Lens.unsnoc'
--
-- >>> pz @Unsnoc [1,2,3,4]
-- Present Just ([1,2,3],4)
-- PresentT (Just ([1,2,3],4))
--
-- >>> pz @Unsnoc []
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @Unsnoc ("xyz" :: T.Text)
-- Present Just ("xy",'z')
-- PresentT (Just ("xy",'z'))
--
data Unsnoc

instance (Show (ConsT s)
        , Show s
        , Snoc s s (ConsT s) (ConsT s)
        ) => P Unsnoc s where
  type PP Unsnoc s = Maybe (s,ConsT s)
  eval _ opts as =
    let msg0 = "Unsnoc"
        b = as ^? _Snoc
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b as] []

-- | similar to 'null' using 'AsEmpty'
--
-- >>> pz @IsEmpty [1,2,3,4]
-- False
-- FalseT
--
-- >>> pz @IsEmpty []
-- True
-- TrueT
--
-- >>> pz @IsEmpty LT
-- False
-- FalseT
--
-- >>> pz @IsEmpty EQ
-- True
-- TrueT
--
data IsEmpty

instance (Show as, AsEmpty as) => P IsEmpty as where
  type PP IsEmpty as = Bool
  eval _ opts as =
    let b = has _Empty as
    in pure $ mkNodeB opts b ["IsEmpty" <> show1 opts " | " as] []

data Null' p

instance (Show (t a)
        , Foldable t
        , t a ~ PP p x
        , P p x
        ) => P (Null' p) x where
  type PP (Null' p) x = Bool
  eval _ opts x = do
    let msg0 = "Null"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = null p
        in mkNodeB opts b ["Null" <> show1 opts " | " p] [hh pp]

-- | similar to 'null' using 'Foldable'
--
-- >>> pz @Null [1,2,3,4]
-- False
-- FalseT
--
-- >>> pz @Null []
-- True
-- TrueT
--
-- >>> pz @Null Nothing
-- True
-- TrueT
--
data Null
type NullT = Null' Id
instance P NullT a => P Null a where
  type PP Null a = PP NullT a
  eval _ = eval (Proxy @NullT)
{-
instance (Show (t a)
        , Foldable t
        , t a ~ as
        ) => P Null as where
  type PP Null as = Bool
  eval _ opts as =
    let b = null as
    in pure $ mkNodeB opts b ["Null" <> show1 opts " | " as] []
-}
-- | similar to 'enumFromTo'
--
-- >>> pz @(EnumFromTo 2 5) ()
-- Present [2,3,4,5]
-- PresentT [2,3,4,5]
--
-- >>> pz @(EnumFromTo 'LT 'GT) ()
-- Present [LT,EQ,GT]
-- PresentT [LT,EQ,GT]
--
-- >>> pz @(EnumFromTo 'GT 'LT) ()
-- Present []
-- PresentT []
--
-- >>> pz @(EnumFromTo (Pred Id) (Succ Id)) (SG.Max 10)
-- Present [Max {getMax = 9},Max {getMax = 10},Max {getMax = 11}]
-- PresentT [Max {getMax = 9},Max {getMax = 10},Max {getMax = 11}]
--
data EnumFromTo p q

instance (P p x
        , P q x
        , PP p x ~ a
        , Show a
        , PP q x ~ a
        , Enum a
        ) => P (EnumFromTo p q) x where
  type PP (EnumFromTo p q) x = [PP p x]
  eval _ opts z = do
    let msg0 = "EnumFromTo"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) -> mkNode opts (PresentT (enumFromTo p q)) [msg0 <> " [" <> show p <> " .. " <> show q <> "]"] [hh pp, hh qq]

-- | similar to 'partitionEithers'
--
-- >>> pz @PartitionEithers [Left 'a',Right 2,Left 'c',Right 4,Right 99]
-- Present ("ac",[2,4,99])
-- PresentT ("ac",[2,4,99])
--
-- >>> pz @PartitionEithers [Right 2,Right 4,Right 99]
-- Present ([],[2,4,99])
-- PresentT ([],[2,4,99])
--
-- >>> pz @PartitionEithers [Left 'a',Left 'c']
-- Present ("ac",[])
-- PresentT ("ac",[])
--
-- >>> pz @PartitionEithers ([] :: [Either () Int])
-- Present ([],[])
-- PresentT ([],[])
--
data PartitionEithers

instance (Show a, Show b) => P PartitionEithers [Either a b] where
  type PP PartitionEithers [Either a b] = ([a], [b])
  eval _ opts as =
    let msg0 = "PartitionEithers"
        b = partitionEithers as
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b as] []

-- | similar to 'partitionThese'. returns a 3-tuple with the results so use 'Fst' 'Snd' 'Thd' to extract
--
-- >>> pz @PartitionThese [This 'a', That 2, This 'c', These 'z' 1, That 4, These 'a' 2, That 99]
-- Present ("ac",[2,4,99],[('z',1),('a',2)])
-- PresentT ("ac",[2,4,99],[('z',1),('a',2)])
--
data PartitionThese

instance (Show a, Show b) => P PartitionThese [These a b] where
  type PP PartitionThese [These a b] = ([a], [b], [(a, b)])
  eval _ opts as =
    let msg0 = "PartitionThese"
        b = partitionThese as
    in pure $ mkNode opts (PresentT b) [show01 opts msg0 b as] []

data Thiss
type ThissT = Fst PartitionThese

instance P ThissT x => P Thiss x where
  type PP Thiss x = PP ThissT x
  eval _ = eval (Proxy @ThissT)

data Thats
type ThatsT = Snd PartitionThese

instance P ThatsT x => P Thats x where
  type PP Thats x = PP ThatsT x
  eval _ = eval (Proxy @ThatsT)

data Theses
type ThesesT = Thd PartitionThese

instance P ThesesT x => P Theses x where
  type PP Theses x = PP ThesesT x
  eval _ = eval (Proxy @ThesesT)

-- want to pass Proxy b to q but then we have no way to calculate 'b'

-- | similar to 'scanl'
--
-- >>> pz @(Scanl (Snd Id :+ Fst Id) (Fst Id) (Snd Id)) ([99],[1..5])
-- Present [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]
-- PresentT [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]
--
-- >>> pz @(ScanN 4 Id (Succ Id)) 'c'
-- Present "cdefg"
-- PresentT "cdefg"
--
-- >>> pz @(FoldN 4 Id (Succ Id)) 'c'
-- Present 'g'
-- PresentT 'g'
--

data Scanl p q r
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- result is scanl but signature is flipped ((a,b) -> b) -> b -> [a] -> [b]

instance (PP p (b,a) ~ b
        , PP q x ~ b
        , PP r x ~ [a]
        , P p (b,a)
        , P q x
        , P r x
        , Show b
        , Show a
        )
     => P (Scanl p q r) x where
  type PP (Scanl p q r) x = [PP q x]
  eval _ opts z = do
    let msg0 = "Scanl"
    lr <- runPQ msg0 (Proxy @q) (Proxy @r) opts z []
    case lr of
      Left e -> pure e
      Right (q,r,qq,rr) -> do
        case chkSize opts msg0 r [hh rr] of
          Left e -> pure e
          Right () -> do
            let msg1 = msg0  -- <> show0 opts " " q <> show0 opts " " r
                ff i b as' rs
                   | i >= _MX = pure (rs, Left $ mkNode opts (FailT (msg1 <> ":failed at i=" <> showIndex i)) [msg1 <> " i=" <> showIndex i <> " (b,as')=" <> show (b,as')] [])
                   | otherwise =
                       case as' of
                         [] -> pure (rs, Right ()) -- ++ [((i,q), mkNode opts (PresentT q) [msg1 <> "(done)"] [])], Right ())
                         a:as -> do
                            pp :: TT b <- eval (Proxy @p) opts (b,a)
                            case getValueLR opts (msg1 <> " i=" <> showIndex i <> " a=" <> show a) pp [] of
                               Left e  -> pure (rs,Left e)
                               Right b' -> ff (i+1) b' as (rs ++ [((i,b), pp)])
            (ts,lrx) :: ([((Int, b), TT b)], Either (TT [b]) ()) <- ff 1 q r []
            pure $ case splitAndAlign opts [msg1] (((0,q), mkNode opts (PresentT q) [msg1 <> "(initial)"] []) : ts) of
                 Left _e -> errorInProgram "Scanl"
                 Right abcs ->
                   let vals = map (view _1) abcs
                       itts = map (view _2 &&& view _3) abcs
                   in case lrx of
                        Left e -> mkNode opts (_tBool e) [msg1] (hh qq : hh rr : map (hh . fixit) itts ++ [hh e])
                        Right () -> mkNode opts (PresentT vals) [show01' opts msg1 vals "b=" q <> show1 opts " | as=" r] (hh qq : hh rr : map (hh . fixit) itts)

data ScanN n p q
type ScanNT n p q = Scanl (Fst Id >> q) p (EnumFromTo 1 n) -- n times using q then run p

instance P (ScanNT n p q) x => P (ScanN n p q) x where
  type PP (ScanN n p q) x = PP (ScanNT n p q) x
  eval _ = eval (Proxy @(ScanNT n p q))

data ScanNA q
type ScanNAT q = ScanN (Fst Id) (Snd Id) q

instance P (ScanNAT q) x => P (ScanNA q) x where
  type PP (ScanNA q) x = PP (ScanNAT q) x
  eval _ = eval (Proxy @(ScanNAT q))

data FoldN n p q
type FoldNT n p q = Last (ScanN n p q)

instance P (FoldNT n p q) x => P (FoldN n p q) x where
  type PP (FoldN n p q) x = PP (FoldNT n p q) x
  eval _ = eval (Proxy @(FoldNT n p q))

data FoldL p q r
type FoldLT p q r = Last (Scanl p q r)

instance P (FoldLT p q r) x => P (FoldL p q r) x where
  type PP (FoldL p q r) x = PP (FoldLT p q r) x
  eval _ = eval (Proxy @(FoldLT p q r))

-- | similar to 'unfoldr'
--
-- >>> pz @(Unfoldr (MaybeBool (Not Null) (SplitAt 2 Id)) Id) [1..5]
-- Present [[1,2],[3,4],[5]]
-- PresentT [[1,2],[3,4],[5]]
--
-- >>> pz @(IterateN 4 (Succ Id)) 4
-- Present [4,5,6,7]
-- PresentT [4,5,6,7]
--
data Unfoldr p q
--type IterateN (t :: Type) n f = Unfoldr (If (Fst Id == 0) (MkNothing t) (Snd Id &&& (Pred Id *** f) >> MkJust Id)) '(n, Id)

instance (PP q a ~ s
        , PP p s ~ Maybe (b,s)
        , P q a
        , P p s
        , Show s
        , Show b
          )
     => P (Unfoldr p q) a where
  type PP (Unfoldr p q) a = [UnfoldT (PP p (PP q a))]
  eval _ opts z = do
    let msg0 = "Unfoldr"
    qq <- eval (Proxy @q) opts z
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let msg1 = msg0 <> show0 opts " " q
            ff i s rs | i >= _MX = pure (rs, Left $ mkNode opts (FailT (msg1 <> ":failed at i=" <> showIndex i)) [msg1 <> " i=" <> showIndex i <> " s=" <> show s] [])
                      | otherwise = do
                              pp :: TT (PP p s) <- eval (Proxy @p) opts s
                              case getValueLR opts (msg1 <> " i=" <> showIndex i <> " s=" <> show s) pp [] of
                                   Left e  -> pure (rs, Left e)
                                   Right Nothing -> pure (rs, Right ())
                                   Right w@(Just (_b,s')) -> ff (i+1) s' (rs ++ [((i,w), pp)])
        (ts,lr) :: ([((Int, PP p s), TT (PP p s))], Either (TT [b]) ()) <- ff 1 q []
        pure $ case splitAndAlign opts [msg1] ts of
             Left _e -> errorInProgram "Unfoldr"
             Right abcs ->
               let vals = map (view _1) abcs
                   itts = map (view _2 &&& view _3) abcs
               in case lr of
                   Left e -> mkNode opts (_tBool e) [msg1] (hh qq : map (hh . fixit) itts ++ [hh e])
                   Right () ->
                     let ret = fst <$> catMaybes vals
                     in mkNode opts (PresentT ret) [show01' opts msg1 ret "s=" q ] (hh qq : map (hh . fixit) itts)

type family UnfoldT mbs where
  UnfoldT (Maybe (b,s)) = b

data IterateN n f
type IterateNT n f = Unfoldr (MaybeBool (Fst Id > 0) '(Snd Id, Pred Id *** f)) '(n, Id)

instance P (IterateNT n f) x => P (IterateN n f) x where
  type PP (IterateN n f) x = PP (IterateNT n f) x
  eval _ = eval (Proxy @(IterateNT n f))

data IterateUntil p f
type IterateUntilT p f = IterateWhile (Not p) f

instance P (IterateUntilT p f) x => P (IterateUntil p f) x where
  type PP (IterateUntil p f) x = PP (IterateUntilT p f) x
  eval _ = eval (Proxy @(IterateUntilT p f))

data IterateWhile p f
type IterateWhileT p f = Unfoldr (MaybeBool p '(Id, f)) Id

instance P (IterateWhileT p f) x => P (IterateWhile p f) x where
  type PP (IterateWhile p f) x = PP (IterateWhileT p f) x
  eval _ = eval (Proxy @(IterateWhileT p f))

data IterateNWhile n p f
type IterateNWhileT n p f = '(n, Id) >> IterateWhile (Fst Id > 0 && (Snd Id >> p)) (Pred Id *** f) >> Map (Snd Id) Id

instance P (IterateNWhileT n p f) x => P (IterateNWhile n p f) x where
  type PP (IterateNWhile n p f) x = PP (IterateNWhileT n p f) x
  eval _ = eval (Proxy @(IterateNWhileT n p f))

data IterateNUntil n p f
type IterateNUntilT n p f = IterateNWhile n (Not p) f

instance P (IterateNUntilT n p f) x => P (IterateNUntil n p f) x where
  type PP (IterateNUntil n p f) x = PP (IterateNUntilT n p f) x
  eval _ = eval (Proxy @(IterateNUntilT n p f))

-- | similar to 'map'
--
-- >>> pz @(Map (Pred Id) Id) [1..5]
-- Present [0,1,2,3,4]
-- PresentT [0,1,2,3,4]
--
data Map p q

instance (Show (PP p a)
        , P p a
        , PP q x ~ f a
        , P q x
        , Show a
        , Show (f a)
        , Foldable f
        ) => P (Map p q) x where
  type PP (Map p q) x = [PP p (ExtractAFromTA (PP q x))]
  eval _ opts x = do
    let msg0 = "Map"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> eval (Proxy @p) opts a) [0::Int ..] (toList q)
        pure $ case splitAndAlign opts [msg0] ts of
             Left e -> e
             Right abcs ->
               let vals = map (view _1) abcs
               in mkNode opts (PresentT vals) [show01 opts msg0 vals q] (hh qq : map (hh . fixit) ts)

data ConcatMap p q
type ConcatMapT p q = Concat (Map p q)

instance P (ConcatMapT p q) x => P (ConcatMap p q) x where
  type PP (ConcatMap p q) x = PP (ConcatMapT p q) x
  eval _ = eval (Proxy @(ConcatMapT p q))

-- | if p then run q else run r
--
-- >>> pz @(If (Gt 4) "greater than 4" "less than or equal to 4" ) 10
-- Present "greater than 4"
-- PresentT "greater than 4"
--
-- >>> pz @(If (Gt 4) "greater than 4" "less than or equal to 4") 0
-- Present "less than or equal to 4"
-- PresentT "less than or equal to 4"
data If p q r

instance (Show (PP r a)
        , P p a
        , PP p a ~ Bool
        , P q a
        , P r a
        , PP q a ~ PP r a
        ) => P (If p q r) a where
  type PP (If p q r) a = PP q a
  eval _ opts a = do
    let msg0 = "If"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts (msg0 <> " condition failed") pp [] of
      Left e -> pure e
      Right b -> do
        qqrr <- if b
              then eval (Proxy @q) opts a
              else eval (Proxy @r) opts a
        pure $ case getValueLR opts (msg0 <> " [" <> show b <> "]") qqrr [hh pp, hh qqrr] of
          Left e -> e
          Right ret -> mkNode opts (_tBool qqrr) [msg0 <> " " <> if b then "(true cond)" else "(false cond)" <> show0 opts " " ret] [hh pp, hh qqrr]

-- | creates a list of overlapping pairs of elements. requires two or more elements
--
-- >>> pz @Pairs [1,2,3,4]
-- Present [(1,2),(2,3),(3,4)]
-- PresentT [(1,2),(2,3),(3,4)]
--
-- >>> pz @Pairs []
-- Error Pairs no data found
-- FailT "Pairs no data found"
--
-- >>> pz @Pairs [1]
-- Error Pairs only one element found
-- FailT "Pairs only one element found"
--
data Pairs
instance Show a => P Pairs [a] where
  type PP Pairs [a] = [(a,a)]
  eval _ opts as =
    let msg0 = "Pairs"
        lr = case as of
               [] -> Left (msg0 <> " no data found")
               [_] -> Left (msg0 <> " only one element found")
               _:bs@(_:_) -> Right (zip as bs)
    in pure $ case lr of
         Left e -> mkNode opts (FailT e) [e] []
         Right zs -> mkNode opts (PresentT zs) [show01 opts msg0 zs as ] []


-- | similar to 'partition'
--
-- >>> pz @(Partition (Ge 3) Id) [10,4,1,7,3,1,3,5]
-- Present ([10,4,7,3,3,5],[1,1])
-- PresentT ([10,4,7,3,3,5],[1,1])
--
-- >>> pz @(Partition (Prime Id) Id) [10,4,1,7,3,1,3,5]
-- Present ([7,3,3,5],[10,4,1,1])
-- PresentT ([7,3,3,5],[10,4,1,1])
--
-- >>> pz @(Partition (Ge 300) Id) [10,4,1,7,3,1,3,5]
-- Present ([],[10,4,1,7,3,1,3,5])
-- PresentT ([],[10,4,1,7,3,1,3,5])
--
-- >>> pz @(Partition (Id < 300) Id) [10,4,1,7,3,1,3,5]
-- Present ([10,4,1,7,3,1,3,5],[])
-- PresentT ([10,4,1,7,3,1,3,5],[])
--
data Partition p q

instance (P p x
        , Show x
        , PP q a ~ [x]
        , PP p x ~ Bool
        , P q a
        ) => P (Partition p q) a where
  type PP (Partition p q) a = (PP q a, PP q a)
  eval _ opts a' = do
    let msg0 = "Partition"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
             ts <- zipWithM (\i a -> ((i, a),) <$> evalBool (Proxy @p) opts a) [0::Int ..] q
             pure $ case splitAndAlign opts [msg0] ts of
               Left e -> e
               Right abcs ->
                 let itts = map (view _2 &&& view _3) abcs
                     w0 = partition (view _1) abcs
                     zz1 = (map (view (_2 . _2)) *** map (view (_2 . _2))) w0
                 in mkNode opts (PresentT zz1) [show01' opts msg0 zz1 "s=" q] (hh qq : map (hh . fixit) itts)

data Filter p q
type FilterT p q = Fst (Partition p q)

instance P (FilterT p q) x => P (Filter p q) x where
  type PP (Filter p q) x = PP (FilterT p q) x
  eval _ = eval (Proxy @(FilterT p q))

-- | similar to 'break'
--
-- >>> pz @(Break (Ge 3) Id) [10,4,1,7,3,1,3,5]
-- Present ([],[10,4,1,7,3,1,3,5])
-- PresentT ([],[10,4,1,7,3,1,3,5])
--
-- >>> pz @(Break (Lt 3) Id) [10,4,1,7,3,1,3,5]
-- Present ([10,4],[1,7,3,1,3,5])
-- PresentT ([10,4],[1,7,3,1,3,5])
--
data Break p q

-- only process up to the pivot! only process while Right False
-- a predicate can return PresentP not just TrueP
instance (P p x
        , PP q a ~ [x]
        , PP p x ~ Bool
        , P q a
        ) => P (Break p q) a where
  type PP (Break p q) a = (PP q a, PP q a)
  eval _ opts a' = do
    let msg0 = "Break"
    qq <- eval (Proxy @q) opts a'
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case chkSize opts msg0 q [hh qq] of
          Left e -> pure e
          Right () -> do
            let ff [] zs = pure (zs, [], Nothing) -- [(ia,qq)] extras | the rest of the data | optional last pivot or failure
                ff ((i,a):ias) zs = do
                   pp <- evalBool (Proxy @p) opts a
                   let v = ((i,a), pp)
                   case getValueLR opts msg0 pp [hh qq] of
                     Right False -> ff ias (zs Seq.|> v)
                     Right True -> pure (zs,map snd ias,Just v)
                     Left _ -> pure (zs,map snd ias,Just v)
            (ialls,rhs,mpivot) <- ff (itoList q) Seq.empty
            pure $ case mpivot of
                 Nothing ->
                   mkNode opts (PresentT (map (snd . fst) (toList ialls), rhs))
                           ([msg0] <> ["cnt=" <> show (length ialls, length rhs)])
                           (map (hh . fixit) (toList ialls))
                 Just iall@(ia, tt) ->
                   case getValueLR opts (msg0 <> " predicate failed") tt (hh qq : map (hh . fixit) (toList (ialls Seq.|> iall))) of
                     Right True ->
                       mkNode opts (PresentT (map (snd . fst) (toList ialls), snd ia : rhs))
                               ([msg0] <> ["cnt=" <> show (length ialls, 1+length rhs)])
                               (hh qq : hh tt : map (hh . fixit) (toList (ialls Seq.|> iall)))

                     Right False -> errorInProgram "Break"
                     Left e -> e

data Span p q
type SpanT p q = Break (Not p) q

instance P (SpanT p q) x => P (Span p q) x where
  type PP (Span p q) x = PP (SpanT p q) x
  eval _ = eval (Proxy @(SpanT p q))

-- | Fails the computation with a message
--
-- >>> pz @(Failt Int (PrintF "value=%03d" Id)) 99
-- Error value=099
-- FailT "value=099"
--
-- >>> pz @(FailS (PrintT "value=%03d string=%s" Id)) (99,"somedata")
-- Error value=099 string=somedata
-- FailT "value=099 string=somedata"
--
data Fail t prt

instance (P prt a
        , PP prt a ~ String
        ) => P (Fail t prt) a where
  type PP (Fail t prt) a = PP t a
  eval _ opts a = do
    let msg0 = "Fail"
    pp <- eval (Proxy @prt) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s -> mkNode opts (FailT s) [msg0 <> " " <> s] (if isVerbose opts then [hh pp] else [])

data FailS p
instance P (Fail I p) x => P (FailS p) x where
  type PP (FailS p) x = PP (Fail I p) x
  eval _ = eval (Proxy @(Fail I p))

data Failt (t :: Type) p
instance P (Fail (Hole t) p) x => P (Failt t p) x where
  type PP (Failt t p) x = PP (Fail (Hole t) p) x
  eval _ = eval (Proxy @(Fail (Hole t) p))

data Failp p
instance P (Fail Unproxy p) x => P (Failp p) x where
  type PP (Failp p) x = PP (Fail Unproxy p) x
  eval _ = eval (Proxy @(Fail Unproxy p))

data Hole (t :: Type)

-- | Acts as a proxy in this dsl where you can explicitly set the Type.
--
--  It is passed around as an argument to help the type checker when needed.
--  see 'ParseTimeP', 'ReadBase'
--
instance Typeable t => P (Hole t) a where
  type PP (Hole t) a = t -- can only be Type not Type -> Type (can use Proxy but then we go down the rabbithole)
  eval _ opts _a =
    let msg0 = "Hole(" <> showT @t <> ")"
    in pure $ mkNode opts (FailT msg0) [msg0 <> " you probably meant to get access to the type of PP only and not evaluate"] []

data Unproxy

instance Typeable a => P Unproxy (Proxy (a :: Type)) where
  type PP Unproxy (Proxy a) = a
  eval _ opts _a =
    let msg0 = "Unproxy(" <> showT @a <> ")"
    in pure $ mkNode opts (FailT msg0) [msg0 <> " you probably meant to get access to the type of PP only and not evaluate"] []

-- | catch a failure
--
-- >>> pz @(Catch (Succ Id) (Fst Id >> Second (ShowP Id) >> PrintT "%s %s" Id >> 'LT)) GT
-- Present LT
-- PresentT LT
--
-- >>> pz @(Catch' (Succ Id) (Second (ShowP Id) >> PrintT "%s %s" Id)) GT
-- Error Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT
-- FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT"
--
-- >>> pz @(Catch' (Succ Id) (Second (ShowP Id) >> PrintT "%s %s" Id)) LT
-- Present EQ
-- PresentT EQ
--
-- more flexible: takes a (String,x) and a proxy so we can still call 'False 'True
-- now takes the FailT string and x so you can print more detail if you want
-- need the proxy so we can fail without having to explicitly specify a type
data Catch p q -- catch p and if fails runs q only on failt

data Catch' p s
type CatchT' p s = Catch p (FailCatch s) -- eg set eg s=PrintF "%d" Id or PrintF "%s" (ShowP Id)
type FailCatch s = Fail (Snd Id >> Unproxy) (Fst Id >> s)

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
            Right _ -> mkNode opts (_tBool qq) [msg0 <> " caught exception[" <> emsg <> "]"] [hh pp, hh qq]
      Right _ -> pure $ mkNode opts (_tBool pp) [msg0 <> " did not fire"] [hh pp]

-- | similar to 'even'
--
-- >>> pz @(Map Even Id) [9,-4,12,1,2,3]
-- Present [False,True,True,False,True,False]
-- PresentT [False,True,True,False,True,False]
--
-- >>> pz @(Map '(Even,Odd) Id) [9,-4,12,1,2,3]
-- Present [(False,True),(True,False),(True,False),(False,True),(True,False),(False,True)]
-- PresentT [(False,True),(True,False),(True,False),(False,True),(True,False),(False,True)]
--
data Even
type EvenT = Mod I 2 == 0

instance P EvenT x => P Even x where
  type PP Even x = PP EvenT x
  eval _ = eval (Proxy @EvenT)

data Odd
type OddT = Mod I 2 == 1

instance P OddT x => P Odd x where
  type PP Odd x = PP OddT x
  eval _ = eval (Proxy @OddT)


--type Div' p q = Fst (DivMod p q)
--type Mod' p q = Snd (DivMod p q)

-- | similar to 'div'
--
-- >>> pz @(Div (Fst Id) (Snd Id)) (10,4)
-- Present 2
-- PresentT 2
--
-- >>> pz @(Div (Fst Id) (Snd Id)) (10,0)
-- Error Div zero denominator
-- FailT "Div zero denominator"
--
data Div p q
instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (Div p q) a where
  type PP (Div p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Div"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg0 <> " zero denominator")) [msg0 <> " zero denominator"] hhs
              _ -> let d = p `div` q
                   in mkNode opts (PresentT d) [show p <> " `div` " <> show q <> " = " <> show d] hhs


-- | similar to 'mod'
--
-- >>> pz @(Mod (Fst Id) (Snd Id)) (10,3)
-- Present 1
-- PresentT 1
--
-- >>> pz @(Mod (Fst Id) (Snd Id)) (10,0)
-- Error Mod zero denominator
-- FailT "Mod zero denominator"
--
data Mod p q
instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (Mod p q) a where
  type PP (Mod p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Mod"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg0 <> " zero denominator")) [msg0 <> " zero denominator"] hhs
              _ -> let d = p `mod` q
                   in mkNode opts (PresentT d) [show p <> " `mod` " <> show q <> " = " <> show d] hhs

-- | similar to 'divMod'
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,3)
-- Present (3,1)
-- PresentT (3,1)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,-3)
-- Present (-4,-2)
-- PresentT (-4,-2)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (-10,3)
-- Present (-4,2)
-- PresentT (-4,2)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (-10,-3)
-- Present (3,-1)
-- PresentT (3,-1)
--
-- >>> pz @(DivMod (Fst Id) (Snd Id)) (10,0)
-- Error DivMod zero denominator
-- FailT "DivMod zero denominator"
--
data DivMod p q

instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (DivMod p q) a where
  type PP (DivMod p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "DivMod"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg0 <> " zero denominator")) [msg0 <> " zero denominator"] hhs
             _ -> let d = p `divMod` q
                  in mkNode opts (PresentT d) [show p <> " `divMod` " <> show q <> " = " <> show d] hhs

-- | similar to 'quotRem'
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,3)
-- Present (3,1)
-- PresentT (3,1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,-3)
-- Present (-3,1)
-- PresentT (-3,1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (-10,-3)
-- Present (3,-1)
-- PresentT (3,-1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (-10,3)
-- Present (-3,-1)
-- PresentT (-3,-1)
--
-- >>> pz @(QuotRem (Fst Id) (Snd Id)) (10,0)
-- Error QuotRem zero denominator
-- FailT "QuotRem zero denominator"
--
data QuotRem p q

instance (PP p a ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        , Integral (PP p a)
        ) => P (QuotRem p q) a where
  type PP (QuotRem p q) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "QuotRem"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg0 <> " zero denominator")) [msg0 <> " zero denominator"] hhs
             _ -> let d = p `quotRem` q
                  in mkNode opts (PresentT d) [show p <> " `quotRem` " <> show q <> " = " <> show d] hhs

data Quot p q
type QuotT p q = Fst (QuotRem p q)

instance P (QuotT p q) x => P (Quot p q) x where
  type PP (Quot p q) x = PP (QuotT p q) x
  eval _ = eval (Proxy @(QuotT p q))

data Rem p q
type RemT p q = Snd (QuotRem p q)

instance P (RemT p q) x => P (Rem p q) x where
  type PP (Rem p q) x = PP (RemT p q) x
  eval _ = eval (Proxy @(RemT p q))

--type OneP = Guard "expected list of length 1" (Len == 1) >> Head Id
--type OneP = Guard (PrintF "expected list of length 1 but found length=%d" Len) (Len == 1) >> Head Id

-- k or prt has access to (Int,a) where Int is the current guard position: hence need to use PrintT
-- todo: better explanation of how this works
-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out)

-- | Guards contain a type level list of tuples the action to run on failure of the predicate and the predicate itself
-- Each tuple validating against the corresponding value in a value list
--
-- \'prt\' receives (Int,a) as input which is the position and value if there is a failure
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 4)]) [17,4]
-- Present [17,4]
-- PresentT [17,4]
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 5)]) [17,4]
-- Error arg2 failed
-- FailT "arg2 failed"
--
-- >>> pz @(Guards '[ '("arg1 failed",Gt 99), '("arg2 failed", Same 4)]) [17,4]
-- Error arg1 failed
-- FailT "arg1 failed"
--
-- >>> pz @(Guards '[ '(PrintT "arg %d failed with value %d" Id,Gt 4), '(PrintT "%d %d" Id, Same 4)]) [17,3]
-- Error 1 3
-- FailT "1 3"
--
-- >>> pz @(GuardsQuick (PrintT "arg %d failed with value %d" Id) '[Gt 4, Ge 3, Same 4]) [17,3,5]
-- Error arg 2 failed with value 5
-- FailT "arg 2 failed with value 5"
--
-- >>> pz @(GuardsQuick (PrintT "arg %d failed with value %d" Id) '[Gt 4, Ge 3, Same 4]) [17,3,5,99]
-- Error Guards: invalid length:expected 3 but found 4
-- FailT "Guards: invalid length:expected 3 but found 4"
--
data GuardsImpl (n :: Nat) (os :: [(k,k1)])

data Guards (ps :: [(k,k1)])

instance (GetLen ps, P (GuardsImpl (LenT ps) ps) [a]) => P (Guards ps) [a] where
  type PP (Guards ps) [a] = PP (GuardsImpl (LenT ps) ps) [a]
  eval _ opts as = do
    let msg0 = "Guards"
        n = getLen @ps
    if n /= length as then
       --let msg1 = msg0 <> ": predicates(" <> show n <> ") /= data(" <> show (length as) <> ")"
       let msg1 = msg0 <> ": invalid length:expected " ++ show n ++ " but found " ++ show (length as)
       in pure $ mkNode opts (FailT msg1) [msg1] []
    else eval (Proxy @(GuardsImpl (LenT ps) ps)) opts as

instance (KnownNat n
        , Show a
        ) => P (GuardsImpl n ('[] :: [(k,k1)])) [a] where
  type PP (GuardsImpl n ('[] :: [(k,k1)])) [a] = [a]
  eval _ opts as =
    let msg0 = "Guards" <> "(" <> show n <> ")"
        n :: Int = nat @n
    in if not (null as) then errorInProgram $ "GuardsImpl base case has extra data " ++ show as
       else pure $ mkNode opts (PresentT as) [msg0 <> " empty"] []

instance (PP prt (Int, a) ~ String
        , P prt (Int, a)
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (GuardsImpl n ps) [a]
        , PP (GuardsImpl n ps) [a] ~ [a]
        , Show a
        ) => P (GuardsImpl n ('(prt,p) ': ps)) [a] where
  type PP (GuardsImpl n ('(prt,p) ': ps)) [a] = [a]
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Guards" <> "(" <> show cpos <> ")"
         msgbase2 = "Guards"
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBool (Proxy @p) opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts (cpos,a) -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNode opts (FailT msgx) [msgbase1 <> " failed [" <> msgx <> "]" <> show0 opts " " a] (hh pp : if isVerbose opts then [hh qq] else [])
                 Right True -> do
                   if pos == 0 then -- we are at the bottom of the tree
                      pure $ mkNode opts (PresentT [a]) [msgbase2] [hh pp]
                   else do
                     ss <- eval (Proxy @(GuardsImpl n ps)) opts as
                     pure $ case getValueLRHide opts (msgbase1 <> " ok | rhs failed") ss [hh pp] of
                       Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                       Right zs -> (ss & tForest %~ \x -> fromTT pp : x) & tBool .~ PresentT (a:zs)
--                         let tt = mkNode opts (PresentT (a:zs)) [msgbase1] [hh pp, hh ss]
--                         in if top then mkNode opts (PresentT (a:zs)) [msgbase2 <> "("++show n++") done!"] [hh tt]
--                            else tt

         _ -> errorInProgram $ "GuardsImpl n+1 case has no data"

data GuardsQuick (prt :: k) (ps :: [k1])
type GuardsQuickT (prt :: k) (ps :: [k1]) = Guards (ToGuardsT prt ps)

instance P (GuardsQuickT prt ps) x => P (GuardsQuick prt ps) x where
  type PP (GuardsQuick prt ps) x = PP (GuardsQuickT prt ps) x
  eval _ = eval (Proxy @(GuardsQuickT prt ps))

-- | boolean guard which checks a given a list of predicates against the list of values
--
-- prefer 'Bools' as 'BoolsQuick' doesnt give much added value: passes in the index and the value to prt but you already have the index in the message
--
-- pulls the top message from the tree if a predicate is false
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23), '(W "mm",Between 0 59), '(PrintT "<<<%d %d>>>" Id,Between 0 59) ] ) [12,93,14]
-- False (Bool(1) [mm] (93 <= 59))
-- FalseT
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23), '(W "mm",Between 0 59), '(PrintT "<<<%d %d>>>" Id,Between 0 59) ] ) [12,13,94]
-- False (Bool(2) [<<<2 94>>>] (94 <= 59))
-- FalseT
--
-- >>> pl @(Bools '[ '(W "hh",Between 0 23), '(W "mm",Between 0 59), '(PrintT "<<<%d %d>>>" Id,Between 0 59) ] ) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(BoolsQuick "abc" '[Between 0 23, Between 0 59, Between 0 59]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(BoolsQuick (PrintT "id=%d val=%d" Id) '[Between 0 23, Between 0 59, Between 0 59]) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(BoolsQuick (PrintT "id=%d val=%d" Id) '[Between 0 23, Between 0 59, Between 0 59]) [12,13,99]
-- False (Bool(2) [id=2 val=99] (99 <= 59))
-- FalseT
--
-- >>> pl @(Bools '[ '("hours",Between 0 23), '("minutes",Between 0 59), '("seconds",Between 0 59) ] ) [12,13,14]
-- True (Bools)
-- TrueT
--
-- >>> pl @(Bools '[ '("hours",Between 0 23), '("minutes",Between 0 59), '("seconds",Between 0 59) ] ) [12,60,14]
-- False (Bool(1) [minutes] (60 <= 59))
-- FalseT
--
-- >>> pl @(Bools '[ '("hours",Between 0 23), '("minutes",Between 0 59), '("seconds",Between 0 59) ] ) [12,60,14,20]
-- False (Bools: invalid length:expected 3 but found 4)
-- FalseT
--
data Bools (ps :: [(k,k1)])

instance (GetLen ps
        , P (BoolsImpl (LenT ps) ps) [a]
        , PP (BoolsImpl (LenT ps) ps) [a] ~ Bool
        ) => P (Bools ps) [a] where
  type PP (Bools ps) [a] = Bool
  eval _ opts as = do
    let msg0 = "Bools"
        msg1 = "Bool("++show n++")"
        n = getLen @ps
    case chkSize opts msg1 as [] of
      Left e -> pure e
      Right () -> do
        if n /= length as then
           let msg2 = msg0 <> ": invalid length:expected " ++ show n ++ " but found " ++ show (length as)
               -- msg2 = predicates(" <> show n <> ") /= data(" <> show (length as) <> ")"
           in pure $ mkNodeB opts False [msg2] [] -- was FailT but now just FalseT
        else evalBool (Proxy @(BoolsImpl (LenT ps) ps)) opts as

data BoolsImpl (n :: Nat) (os :: [(k,k1)])

instance (KnownNat n
        , Show a
        ) => P (BoolsImpl n ('[] :: [(k,k1)])) [a] where
  type PP (BoolsImpl n ('[] :: [(k,k1)])) [a] = Bool
  eval _ opts as =
    let msg0 = "Bools(" <> show n <> ")"
        n :: Int = nat @n
    in if not (null as) then errorInProgram $ "BoolsImpl base case has extra data " ++ show as
       else pure $ mkNodeB opts True [msg0 <> " empty"] []

instance (PP prt (Int, a) ~ String
        , P prt (Int, a)
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (BoolsImpl n ps) [a]
        , PP (BoolsImpl n ps) [a] ~ Bool
--        , Show a
        ) => P (BoolsImpl n ('(prt,p) ': ps)) [a] where
  type PP (BoolsImpl n ('(prt,p) ': ps)) [a] = Bool
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Bool(" <> showIndex cpos <> ")"
         msgbase2 = "Bools"
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBool (Proxy @p) opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts (cpos,a) -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNodeB opts False [msgbase1 <> " [" <> msgx <> "] " <> topMessage pp] (hh pp : if isVerbose opts then [hh qq] else [])
                 Right True -> do
                   if pos == 0 then -- we are at the bottom of the tree
                      pure $ mkNodeB opts True [msgbase2] [hh pp]
                   else do
                     ss <- evalBool (Proxy @(BoolsImpl n ps)) opts as
                     pure $ case getValueLRHide opts (msgbase1 <> " ok | rhs failed") ss [hh pp] of
                       Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                       Right _ ->  ss & tForest %~ \x -> fromTT pp : x
         _ -> errorInProgram $ "BoolsImpl n+1 case has no data"

data BoolsQuick (prt :: k) (ps :: [k1])
type BoolsQuickT (prt :: k) (ps :: [k1]) = Bools (ToGuardsT prt ps)

instance P (BoolsQuickT prt ps) x => P (BoolsQuick prt ps) x where
  type PP (BoolsQuick prt ps) x = PP (BoolsQuickT prt ps) x
  eval _ = eval (Proxy @(BoolsQuickT prt ps))

-- | leverages 'RepeatT' for repeating predicates (passthrough method)
--
-- >>> pl @(BoolsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255)) [121,33,7,256]
-- False (Bool(3) [id=3 must be between 0 and 255, found 256] (256 <= 255))
-- FalseT
--
-- >>> pl @(BoolsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255)) [121,33,7,44]
-- True (Bools)
-- TrueT
--
data BoolsN prt (n :: Nat) p
type BoolsNT prt (n :: Nat) p = Bools (ToGuardsT prt (RepeatT n p))

instance P (BoolsNT prt n p) [a] => P (BoolsN prt n p) [a] where
  type PP (BoolsN prt n p) [a] = PP (BoolsNT prt n p) [a]
  eval _ = eval (Proxy @(BoolsNT prt n p))

-- | if a predicate fails then then the corresponding symbol and value will be passed to the print function
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23),'("minutes",Between 0 59),'("seconds",Between 0 59)]) [13,59,61]
-- Error seconds invalid: found 61
-- FailT "seconds invalid: found 61"
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23),'("minutes",Between 0 59),'("seconds",Between 0 59)]) [27,59,12]
-- Error hours invalid: found 27
-- FailT "hours invalid: found 27"
--
-- >>> pz @(GuardsDetail "%s invalid: found %d" '[ '("hours", Between 0 23),'("minutes",Between 0 59),'("seconds",Between 0 59)]) [23,59,12]
-- Present [23,59,12]
-- PresentT [23,59,12]
--
data GuardsImplXX (ps :: [(k,k1)])

instance (GetLen ps
        , P (GuardsImplX (LenT ps) ps) [a]
        ) => P (GuardsImplXX ps) [a] where
  type PP (GuardsImplXX ps) [a] = PP (GuardsImplX (LenT ps) ps) [a]
  eval _ opts as = do
    let msg0 = "Guards"
        n = getLen @ps
    if n /= length as then
       let msg1 = msg0 <> ": invalid length:expected " ++ show n ++ " but found " ++ show (length as)
       in pure $ mkNode opts (FailT msg1) [msg1] []
    else eval (Proxy @(GuardsImplX (LenT ps) ps)) opts as

data GuardsImplX (n :: Nat) (os :: [(k,k1)])

instance Show a
        => P (GuardsImplX n ('[] :: [(k,k1)])) [a] where
  type PP (GuardsImplX n ('[] :: [(k,k1)])) [a] = [a]
  eval _ opts as =
    let msg0 = "Guards"
        -- n :: Int = nat @n
    in if not (null as) then errorInProgram $ "GuardsImplX base case has extra data " ++ show as
       else pure $ mkNode opts (PresentT as) [msg0] []

instance (PP prt a ~ String
        , P prt a
        , KnownNat n
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (GuardsImplX n ps) [a]
        , PP (GuardsImplX n ps) [a] ~ [a]
        , Show a
        ) => P (GuardsImplX n ('(prt,p) ': ps)) [a] where
  type PP (GuardsImplX n ('(prt,p) ': ps)) [a] = [a]
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase1 = "Guard" <> "(" <> showIndex cpos <> ")"
         msgbase2 = "Guards"
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         a:as -> do
            pp <- evalBool (Proxy @p) opts a
            case getValueLR opts (msgbase1 <> " p failed") pp [] of
                 Left e -> pure e
                 Right False -> do
                   qq <- eval (Proxy @prt) opts a -- only run prt when predicate is False
                   pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                      Left e -> e
                      Right msgx -> mkNode opts (FailT msgx) [msgbase1 <> " failed [" <> msgx <> "]" <> show0 opts " " a] (hh pp : if isVerbose opts then [hh qq] else [])
                 Right True -> do
                   ss <- eval (Proxy @(GuardsImplX n ps)) opts as
                   pure $ case getValueLRHide opts (msgbase1 <> " ok | rhs failed") ss [hh pp] of
                     Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                     Right zs -> mkNode opts (PresentT (a:zs)) [msgbase1 <> show0 opts " " a] [hh pp, hh ss]
         _ -> errorInProgram $ "GuardsImplX n+1 case has no data"

data GuardsDetail prt (ps :: [(k0,k1)])
type GuardsDetailT prt (ps :: [(k0,k1)]) = GuardsImplXX (ToGuardsDetailT prt ps)

instance P (GuardsDetailT prt ps) x => P (GuardsDetail prt ps) x where
  type PP (GuardsDetail prt ps) x = PP (GuardsDetailT prt ps) x
  eval _ = eval (Proxy @(GuardsDetailT prt ps))

type family ToGuardsDetailT (prt :: k1) (os :: [(k2,k3)]) :: [(Type,k3)] where
  ToGuardsDetailT prt '[ '(s,p) ] = '(PrintT prt '(s,Id), p) : '[]
  ToGuardsDetailT prt ( '(s,p) ': ps) = '(PrintT prt '(s,Id), p) ': ToGuardsDetailT prt ps
  ToGuardsDetailT prt '[] = GL.TypeError ('GL.Text "ToGuardsDetailT cannot be empty")

-- | leverages 'RepeatT' for repeating predicates (passthrough method)
--
-- >>> pz @(GuardsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255)) [121,33,7,256]
-- Error id=3 must be between 0 and 255, found 256
-- FailT "id=3 must be between 0 and 255, found 256"
--
-- >>> pz @(GuardsN (PrintT "id=%d must be between 0 and 255, found %d" Id) 4 (Between 0 255)) [121,33,7,44]
-- Present [121,33,7,44]
-- PresentT [121,33,7,44]
--
data GuardsN prt (n :: Nat) p
type GuardsNT prt (n :: Nat) p = Guards (ToGuardsT prt (RepeatT n p))

instance P (GuardsNT prt n p) [a] => P (GuardsN prt n p) [a] where
  type PP (GuardsN prt n p) [a] = PP (GuardsNT prt n p) [a]
  eval _ = eval (Proxy @(GuardsNT prt n p))

-- | \'p\' is the predicate and on failure of the predicate runs \'prt\'
--
-- >>> pz @(Guard "expected > 3" (Gt 3)) 17
-- Present 17
-- PresentT 17
--
-- >>> pz @(Guard "expected > 3" (Gt 3)) 1
-- Error expected > 3
-- FailT "expected > 3"
--
-- >>> pz @(Guard (PrintF "%d not > 3" Id) (Gt 3)) (-99)
-- Error -99 not > 3
-- FailT "-99 not > 3"
--
data Guard prt p

data ExitWhen prt p
type ExitWhenT prt p = Guard prt (Not p)

instance P (ExitWhenT prt p) x => P (ExitWhen prt p) x where
  type PP (ExitWhen prt p) x = PP (ExitWhenT prt p) x
  eval _ = eval (Proxy @(ExitWhenT prt p))

instance (Show a
        , P prt a
        , PP prt a ~ String
        , P p a
        , PP p a ~ Bool
        ) => P (Guard prt p) a where
  type PP (Guard prt p) a = a
  eval _ opts a = do
    let msg0 = "Guard"
    pp <- evalBool (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right False -> do
        qq <- eval (Proxy @prt) opts a
        pure $ case getValueLR opts (msg0 <> " Msg") qq [hh pp] of
          Left e -> e
          Right msgx -> mkNode opts (FailT msgx) [msg0 <> "(failed) [" <> msgx <> "]" <> show0 opts " | " a] (hh pp : if isVerbose opts then [hh qq] else [])
      Right True -> pure $ mkNode opts (PresentT a) [msg0 <> "(ok)" <> show0 opts " | " a] [hh pp]  -- dont show the guard message if successful


-- | similar to 'Guard' but uses the root message of the False predicate case as the failure message
--
-- most uses of GuardSimple can be replaced by using 'ol' and a boolean predicate unless you require failure
--
-- >>> pz @(GuardSimple (Luhn Id)) [1..4]
-- Error (Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])
-- FailT "(Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])"
--
-- >>> pl @(Luhn Id) [1..4]
-- False (Luhn map=[4,6,2,2] sum=14 ret=4 | [1,2,3,4])
-- FalseT
--
-- >>> pz @(GuardSimple (Luhn Id)) [1,2,3,0]
-- Present [1,2,3,0]
-- PresentT [1,2,3,0]
--
-- >>> pz @(GuardSimple (Len > 30)) [1,2,3,0]
-- Error (4 > 30)
-- FailT "(4 > 30)"
--
data GuardSimple p

instance (Show a
        , P p a
        , PP p a ~ Bool
        ) => P (GuardSimple p) a where
  type PP (GuardSimple p) a = a
  eval _ opts a = do
    let msg0 = "GuardSimple"
    pp <- evalBool (Proxy @p) (if hasNoTree opts then o0 else opts) a -- to not lose the message in oLite mode we use non lite and then fix it up after
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right False ->
        let msgx = topMessage pp
        in mkNode opts (FailT msgx) [msg0 <> "(failed) " <> msgx <> show0 opts " | " a] [hh pp]
      Right True ->
        mkNode opts (PresentT a) [msg0 <> "(ok)" <> show0 opts " | " a] [hh pp]


-- | just run the effect but skip the value
-- for example for use with Stdout so it doesnt interfere with the \'a\' on the rhs unless there is an failure
data Skip p

instance (Show (PP p a), P p a) => P (Skip p) a where
  type PP (Skip p) a = a
  eval _ opts a = do
    let msg0 = "Skip"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT a) [msg0 <> show0 opts " " p] [hh pp]

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

-- advantage of (>>) over 'Do [k] is we can use different kinds for (>>) without having to wrap with 'W'

-- | This is composition for predicates
--
-- >>> pz @(Fst Id >> Succ (Id !! 0)) ([11,12],'x')
-- Present 12
-- PresentT 12
--
-- >>> pz @(Len *** Succ Id >> ShowP (First (Pred Id))) ([11,12],'x')
-- Present "(1,'y')"
-- PresentT "(1,'y')"
--
data p >> q
infixr 1 >>

instance (Show (PP p a)
        , Show (PP q (PP p a))
        , P p a
        , P q (PP p a)
        ) => P (p >> q) a where
  type PP (p >> q) a = PP q (PP p a)
  eval _ opts a = do
    let msg0 = "(>>)"
    pp <- eval (Proxy @p) opts a
    case getValueLRHide opts ("(>>) lhs failed") pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts p
        pure $ case getValueLRHide opts (show p <> " (>>) rhs failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (_tBool qq) [lit01 opts msg0 q (topMessage' qq)] [hh pp, hh qq]

data p << q
type LeftArrowsT p q = q >> p
infixr 1 <<

instance P (LeftArrowsT p q) x => P (p << q) x where
  type PP (p << q) x = PP (LeftArrowsT p q) x
  eval _ = eval (Proxy @(LeftArrowsT p q))

type p >>> q = p >> q
infixl 1 >>>

-- | similar to 'Prelude.&&'
--
-- >>> pz @(Fst Id && Snd Id) (True, True)
-- True
-- TrueT
--
-- >>> pz @(Id > 15 && Id < 17) 16
-- True
-- TrueT
--
-- >>> pz @(Id > 15 && Id < 17) 30
-- False
-- FalseT
--
-- >>> pz @(Fst Id && (Length (Snd Id) >= 4)) (True,[11,12,13,14])
-- True
-- TrueT
--
-- >>> pz @(Fst Id && (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- False
-- FalseT
--
data p && q
infixr 3 &&

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p && q) a where
  type PP (p && q) a = Bool
  eval _ opts a = do
    let msg0 = "&&"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True,True) -> ""
                  (False,True) -> topMessage pp
                  (True,False) -> topMessage qq
                  (False,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
        in mkNodeB opts (p&&q) [show p <> " " <> msg0 <> " " <> show q <> (if null zz then zz else " | " <> zz)] [hh pp, hh qq]

-- | similar to 'Prelude.||'
--
-- >>> pz @(Fst Id || (Length (Snd Id) >= 4)) (False,[11,12,13,14])
-- True
-- TrueT
--
-- >>> pz @(Not (Fst Id) || (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- False
-- FalseT
--
data p || q
infixr 2 ||

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p || q) a where
  type PP (p || q) a = Bool
  eval _ opts a = do
    let msg0 = "||"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (False,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p||q) [show p <> " " <> msg0 <> " " <> show q <> (if null zz then zz else " | " <> zz)] [hh pp, hh qq]

-- | implication
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) >= 4)) (True,[11,12,13,14])
-- True
-- TrueT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) == 4)) (True,[12,11,12,13,14])
-- False
-- FalseT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) == 4)) (False,[12,11,12,13,14])
-- True
-- TrueT
--
-- >>> pz @(Fst Id ~> (Length (Snd Id) >= 4)) (False,[11,12,13,14])
-- True
-- TrueT
--
data p ~> q
infixr 1 ~>

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p ~> q) a where
  type PP (p ~> q) a = Bool
  eval _ opts a = do
    let msg0 = "~>"
    lr <- runPQBool msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let zz = case (p,q) of
                  (True,False) -> topMessage pp <> " " <> msg0 <> " " <> topMessage qq
                  _ -> ""
        in mkNodeB opts (p~>q) [show p <> " " <> msg0 <> " " <> show q <> (if null zz then zz else " | " <> zz)] [hh pp, hh qq]


-- | 'not' function
--
-- >>> pz @(Not Id) False
-- True
-- TrueT
--
-- >>> pz @(Not Id) True
-- False
-- FalseT
--
-- >>> pz @(Not (Fst Id)) (True,22)
-- False
-- FalseT
--
-- >>> pl @(Not (Lt 3)) 13
-- True (Not (13 < 3))
-- TrueT
--
data Not p

instance (PP p x ~ Bool, P p x) => P (Not p) x where
  type PP (Not p) x = Bool
  eval _ opts x = do
    let msg0 = "Not"
    pp <- evalBool (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = not p
        in mkNodeB opts b [msg0 <> " " <> topMessage pp] [hh pp]

type OrdP p q = p ==! q

data p ==! q
infix 4 ==!

-- | similar to 'compare'
--
-- >>> pz @(Fst Id ==! Snd Id) (10,9)
-- Present GT
-- PresentT GT
--
-- >>> pz @(14 % 3 ==! Fst Id -% Snd Id) (-10,7)
-- Present GT
-- PresentT GT
--
-- >>> pz @(Fst Id ==! Snd Id) (10,11)
-- Present LT
-- PresentT LT
--
-- >>> pz @(Snd Id ==! (Fst Id >> Snd Id >> Head Id)) (('x',[10,12,13]),10)
-- Present EQ
-- PresentT EQ
--
-- >>> pz @(Snd Id ==! Head (Snd (Fst Id))) (('x',[10,12,13]),10)
-- Present EQ
-- PresentT EQ
--
--type OrdA' p q = (Fst Id >> p) ==! (Snd Id >> q)
--type OrdA p = OrdA' p p

data OrdA p

instance P (OrdA' p p) x => P (OrdA p) x where
  type PP (OrdA p) x = PP (OrdA' p p) x
  eval _ = eval (Proxy @(OrdA' p p))

instance (Ord (PP p a)
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
        in mkNode opts (PresentT d) [msg0 <> " " <> show p <> " " <> prettyOrd d <> show0 opts " " q] [hh pp, hh qq]

data OrdA' p q
type OrdAT' p q = (Fst Id >> p) ==! (Snd Id >> q)

instance P (OrdAT' p q) x => P (OrdA' p q) x where
  type PP (OrdA' p q) x = PP (OrdAT' p q) x
  eval _ = eval (Proxy @(OrdAT' p q))

-- | compare two strings ignoring case
--
-- >>> pz @(Fst Id ===~ Snd Id) ("abC","aBc")
-- Present EQ
-- PresentT EQ
--
-- >>> pz @(Fst Id ===~ Snd Id) ("abC","DaBc")
-- Present LT
-- PresentT LT
--
type OrdI p q = p ===~ q
data p ===~ q
infix 4 ===~

instance (PP p a ~ String
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
        in mkNode opts (PresentT d) [msg0 <> " " <> p <> " " <> prettyOrd d <> " " <> q] [hh pp, hh qq]

-- | compare two values using the given ordering \'o\'
--
-- >>> pl @(Lt 4) 123
-- False (123 < 4)
-- FalseT
--
-- >>> pl @(Lt 4) 1
-- True (1 < 4)
-- TrueT
--
-- >>> pl @(Between (Negate 7) 20) (-4)
-- True (-7 <= -4 <= 20)
-- TrueT
--
-- >>> pl @(Between (Negate 7) 20) 21
-- False (21 <= 20)
-- FalseT
--
data Cmp (o :: OrderingP) p q

instance (GetOrd o
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
        in mkNodeB opts b [show p <> " " <> sfn <> show0 opts " " q] [hh pp, hh qq]

-- | compare two strings ignoring case using the given ordering \'o\'
data CmpI (o :: OrderingP) p q

instance (PP p a ~ String
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
        in mkNodeB opts b ["CmpI " <> p <> " " <> sfn <> " " <> q] [hh pp, hh qq]


-- | similar to 'Control.Lens.itoList'
--
-- >>> pz @(IToList _ Id) ("aBc" :: String)
-- Present [(0,'a'),(1,'B'),(2,'c')]
-- PresentT [(0,'a'),(1,'B'),(2,'c')]
--
data IToList' t p

instance (Show x
        , P p x
        , Typeable (PP t (PP p x))
        , Show (PP t (PP p x))
        , FoldableWithIndex (PP t (PP p x)) f
        , PP p x ~ f a
        , Show a
        ) => P (IToList' t p) x where
  type PP (IToList' t p) x = [(PP t (PP p x), ExtractAFromTA (PP p x))]
  eval _ opts x = do
    let msg0 = "IToList"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = itoList p
            t = showT @(PP t (PP p x))
        in mkNode opts (PresentT b) [msg0 <> "(" <> t <> ")" <> show0 opts " " b <> show1 opts " | " x] [hh pp]

data IToList (t :: Type) p
type IToListT (t :: Type) p = IToList' (Hole t) p

instance P (IToListT t p) x => P (IToList t p) x where
  type PP (IToList t p) x = PP (IToListT t p) x
  eval _ = eval (Proxy @(IToListT t p))

-- | similar to 'toList'
--
-- >>> pz @ToList ("aBc" :: String)
-- Present "aBc"
-- PresentT "aBc"
--
-- >>> pz @ToList (Just 14)
-- Present [14]
-- PresentT [14]
--
-- >>> pz @ToList Nothing
-- Present []
-- PresentT []
--
-- >>> pz @ToList (Left "xx")
-- Present []
-- PresentT []
--
-- >>> pz @ToList (These 12 "xx")
-- Present ["xx"]
-- PresentT ["xx"]
--
data ToList
instance (Show (t a)
        , Foldable t
        , Show a
        ) => P ToList (t a) where
  type PP ToList (t a) = [a]
  eval _ opts as =
    let msg0 = "ToList"
        z = toList as
    in pure $ mkNode opts (PresentT z) [show01 opts msg0 z as] []

-- | similar to 'toList'
--
-- >>> pz @(ToList' Id) ("aBc" :: String)
-- Present "aBc"
-- PresentT "aBc"
--
-- >>> pz @(ToList' Id) (Just 14)
-- Present [14]
-- PresentT [14]
--
-- >>> pz @(ToList' Id) Nothing
-- Present []
-- PresentT []
--
-- >>> pz @(ToList' Id) (Left "xx")
-- Present []
-- PresentT []
--
-- >>> pz @(ToList' Id) (These 12 "xx")
-- Present ["xx"]
-- PresentT ["xx"]
--
data ToList' p

instance (PP p x ~ t a
        , P p x
        , Show (t a)
        , Foldable t
        , Show a
        ) => P (ToList' p) x where
  type PP (ToList' p) x = [ExtractAFromTA (PP p x)] -- extra layer of indirection means pe (ToList' Id) "abc" won't work without setting the type of "abc" unlike ToList
  eval _ opts x = do
    let msg0 = "ToList'"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let hhs = [hh pp]
            b = toList p
        in mkNode opts (PresentT b) [show01 opts msg0 b p] hhs

-- | invokes 'GE.toList'
--
-- >>> pz @ToListExt (M.fromList [(1,'x'),(4,'y')])
-- Present [(1,'x'),(4,'y')]
-- PresentT [(1,'x'),(4,'y')]
--
-- >>> pz @ToListExt (T.pack "abc")
-- Present "abc"
-- PresentT "abc"
--
data ToListExt

instance (Show l
        , GE.IsList l
        , Show (GE.Item l)
        ) => P ToListExt l where
  type PP ToListExt l = [GE.Item l]
  eval _ opts as =
    let msg0 = "ToListExt"
        z = GE.toList as
    in pure $ mkNode opts (PresentT z) [show01 opts msg0 z as] []

data FromList (t :: Type) -- doesnt work with OverloadedLists unless you cast to [a] explicitly

instance (a ~ GE.Item t
        , Show t
        , GE.IsList t
        ) => P (FromList t) [a] where
  type PP (FromList t) [a] = t
  eval _ opts as =
    let msg0 = "FromList"
        z = GE.fromList (as :: [GE.Item t]) :: t
    in pure $ mkNode opts (PresentT z) [msg0 <> show0 opts " " z] []

-- | invokes 'GE.fromList'
--
-- requires the OverloadedLists extension
--
-- >>> :set -XOverloadedLists
-- >>> pz @(FromListExt (M.Map _ _)) [(4,"x"),(5,"dd")]
-- Present fromList [(4,"x"),(5,"dd")]
-- PresentT (fromList [(4,"x"),(5,"dd")])
--
data FromListExt (t :: Type)
-- l ~ l' is key
instance (Show l
        , GE.IsList l
        , l ~ l'
        ) => P (FromListExt l') l where
  type PP (FromListExt l') l = l'
  eval _ opts as =
    let msg0 = "FromListExt"
        z = GE.fromList (GE.toList @l as)
    in pure $ mkNode opts (PresentT z) [msg0 <> show0 opts " " z] []

-- | predicate on 'These'
--
-- >>> pz @(IsThis Id) (This "aBc")
-- True
-- TrueT
--
-- >>> pz @(IsThis Id) (These 1 'a')
-- False
-- FalseT
--
-- >>> pz @(IsThese Id) (These 1 'a')
-- True
-- TrueT
--
data IsTh (th :: These x y) p -- x y can be anything


-- trying to avoid show instance cos of ambiguities
instance (PP p x ~ These a b
        , P p x
        , Show a
        , Show b
        , GetThese th
        ) => P (IsTh (th :: These x1 x2) p) x where
  type PP (IsTh th p) x = Bool
  eval _ opts x = do
    let msg0 = "IsTh"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (t,f) = getThese @th
            b = f p
        in mkNodeB opts b [msg0 <> " " <> t <> show1 opts " | " p] []

data IsThis p
type IsThisT p = IsTh ('This '()) p

instance P (IsThisT p) x => P (IsThis p) x where
  type PP (IsThis p) x = PP (IsThisT p) x
  eval _ = eval (Proxy @(IsThisT p))

data IsThat p
type IsThatT p = IsTh ('That '()) p

instance P (IsThatT p) x => P (IsThat p) x where
  type PP (IsThat p) x = PP (IsThatT p) x
  eval _ = eval (Proxy @(IsThatT p))

data IsThese p
type IsTheseT p = IsTh ('These '() '()) p

instance P (IsTheseT p) x => P (IsThese p) x where
  type PP (IsThese p) x = PP (IsTheseT p) x
  eval _ = eval (Proxy @(IsTheseT p))

-- | similar to 'Data.These.these'
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (This 13)
-- Present 13
-- PresentT 13
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (That "this is a long string")
-- Present 21
-- PresentT 21
--
-- >>> pz @(TheseIn Id Len (Fst Id + Length (Snd Id))) (These 20 "somedata")
-- Present 28
-- PresentT 28
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (That "this is a long string")
-- Present Right "this is a long string"
-- PresentT (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (These 1 "this is a long string")
-- Present Right "this is a long string"
-- PresentT (Right "this is a long string")
--
-- >>> pz @(TheseIn (MkLeft _ Id) (MkRight _ Id) (If (Fst Id > Length (Snd Id)) (MkLeft _ (Fst Id)) (MkRight _ (Snd Id)))) (These 100 "this is a long string")
-- Present Left 100
-- PresentT (Left 100)
--
data TheseIn p q r

instance (Show a
        , Show b
        , Show (PP p a)
        , P p a
        , P q b
        , P r (a,b)
        , PP p a ~ PP q b
        , PP p a ~ PP r (a,b)
        , PP q b ~ PP r (a,b)
         )  => P (TheseIn p q r) (These a b) where
  type PP (TheseIn p q r) (These a b) = PP p a
  eval _ opts =
     \case
        This a -> do
          let msg0 = "This"
          pp <- eval (Proxy @p) opts a
          pure $ case getValueLR opts (msg0 <> " p failed") pp [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [show01' opts msg0 c "This " a] [hh pp]
        That b -> do
          let msg0 = "That"
          qq <- eval (Proxy @q) opts b
          pure $ case getValueLR opts (msg0 <> " q failed") qq [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [show01' opts msg0 c "That " b] [hh qq]
        These a b -> do
          let msg0 = "TheseIn"
          rr <- eval (Proxy @r) opts (a,b)
          pure $ case getValueLR opts (msg0 <> " r failed") rr [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [show01 opts msg0 c (These a b)] [hh rr]

data TheseId p q
type TheseIdT p q = TheseIn '(I, p) '(q, I) I

instance P (TheseIdT p q) x => P (TheseId p q) x where
  type PP (TheseId p q) x = PP (TheseIdT p q) x
  eval _ = eval (Proxy @(TheseIdT p q))
-- | creates an empty list of the given type
--
-- >>> pz @(Id :+ EmptyList _) 99
-- Present [99]
-- PresentT [99]
--
data EmptyList' t

instance P (EmptyList' t) x where
  type PP (EmptyList' t) x = [PP t x]
  eval _ opts _ =
    pure $ mkNode opts (PresentT []) ["EmptyList"] []

data EmptyList (t :: Type)
type EmptyListT (t :: Type) = EmptyList' (Hole t)

instance P (EmptyList t) x where
  type PP (EmptyList t) x = PP (EmptyListT t) x
  eval _ = eval (Proxy @(EmptyListT t))

-- | creates a singleton from a value
--
-- >>> pz @(Singleton (Char1 "aBc")) ()
-- Present "a"
-- PresentT "a"
--
-- >>> pz @(Singleton Id) False
-- Present [False]
-- PresentT [False]
--
-- >>> pz @(Singleton (Snd Id)) (False,"hello")
-- Present ["hello"]
-- PresentT ["hello"]
--
data Singleton p

instance P p x => P (Singleton p) x where
  type PP (Singleton p) x = [PP p x]
  eval _ opts x = do
    let msg0 = "Singleton"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT [p]) [msg0] [hh pp]

--type Singleton p = p :+ EmptyT [] p

-- | extracts the first character from a non empty 'Symbol'
--
-- >>> pz @(Char1 "aBc") ()
-- Present 'a'
-- PresentT 'a'
--
data Char1 (s :: Symbol)  -- gets the first char from the Symbol [requires that Symbol is not empty]
instance (KnownSymbol s, GL.CmpSymbol s "" ~ 'GT) => P (Char1 s) a where
  type PP (Char1 s) a = Char
  eval _ opts _ =
     case symb @s of
       [] -> errorInProgram "Char1: found empty Symbol/string"
       c:_ -> pure $ mkNode opts (PresentT c) ["Char1" <> show0 opts " " c] []

-- | similar to 'Data.Align.align' thats pads with 'Data.These.This' or 'Data.These.That' if one list is shorter than the other
--
-- the key is that all information about both lists are preserved
--
-- >>> pz @(ZipThese (Fst Id) (Snd Id)) ("aBc", [1..5])
-- Present [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
-- PresentT [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
--
-- >>> pz @(ZipThese (Fst Id) (Snd Id)) ("aBcDeF", [1..3])
-- Present [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
-- PresentT [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese Id Reverse) "aBcDeF"
-- Present [These 'a' 'F',These 'B' 'e',These 'c' 'D',These 'D' 'c',These 'e' 'B',These 'F' 'a']
-- PresentT [These 'a' 'F',These 'B' 'e',These 'c' 'D',These 'D' 'c',These 'e' 'B',These 'F' 'a']
--
-- >>> pz @(ZipThese Id '[]) "aBcDeF"
-- Present [This 'a',This 'B',This 'c',This 'D',This 'e',This 'F']
-- PresentT [This 'a',This 'B',This 'c',This 'D',This 'e',This 'F']
--
-- >>> pz @(ZipThese '[] Id) "aBcDeF"
-- Present [That 'a',That 'B',That 'c',That 'D',That 'e',That 'F']
-- PresentT [That 'a',That 'B',That 'c',That 'D',That 'e',That 'F']
--
-- >>> pz @(ZipThese '[] '[]) "aBcDeF"
-- Present []
-- PresentT []
--
data ZipThese p q

instance (PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipThese p q) a where
  type PP (ZipThese p q) a = [These (ExtractAFromList (PP p a)) (ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipThese"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> e
          Right () ->
            let d = simpleAlign p q
            in mkNode opts (PresentT d) [show01' opts msg0 d "p=" p <> show1 opts " | q=" q] hhs

simpleAlign :: [a] -> [b] -> [These a b]
simpleAlign as [] = map This as
simpleAlign [] bs = map That bs
simpleAlign (a:as) (b:bs) = These a b : simpleAlign as bs

type family ExtractAFromTA (ta :: Type) :: Type where
  ExtractAFromTA (t a) = a
  ExtractAFromTA z = GL.TypeError (
      'GL.Text "ExtractAFromTA: expected (t a) but found something else"
      ':$$: 'GL.Text "t a = "
      ':<>: 'GL.ShowType z)

-- todo: get ExtractAFromList failure to fire if wrong Type
-- | extract \'a\' from \'[a]\' which I need for type PP
type family ExtractAFromList (as :: Type) :: Type where
  ExtractAFromList [a] = a
  ExtractAFromList z = GL.TypeError (
      'GL.Text "ExtractAFromList: expected [a] but found something else"
      ':$$: 'GL.Text "as = "
      ':<>: 'GL.ShowType z)


-- | Zip two lists to their maximum length using padding if needed
--
-- >>> pz @(ZipPad (Char1 "Z") 99 (Fst Id) (Snd Id)) ("abc", [1..5])
-- Present [('a',1),('b',2),('c',3),('Z',4),('Z',5)]
-- PresentT [('a',1),('b',2),('c',3),('Z',4),('Z',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 (Fst Id) (Snd Id)) ("abcdefg", [1..5])
-- Present [('a',1),('b',2),('c',3),('d',4),('e',5),('f',99),('g',99)]
-- PresentT [('a',1),('b',2),('c',3),('d',4),('e',5),('f',99),('g',99)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 (Fst Id) (Snd Id)) ("abcde", [1..5])
-- Present [('a',1),('b',2),('c',3),('d',4),('e',5)]
-- PresentT [('a',1),('b',2),('c',3),('d',4),('e',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 (Fst Id) (Snd Id)) ("", [1..5])
-- Present [('Z',1),('Z',2),('Z',3),('Z',4),('Z',5)]
-- PresentT [('Z',1),('Z',2),('Z',3),('Z',4),('Z',5)]
--
-- >>> pz @(ZipPad (Char1 "Z") 99 (Fst Id) (Snd Id)) ("abcde", [])
-- Present [('a',99),('b',99),('c',99),('d',99),('e',99)]
-- PresentT [('a',99),('b',99),('c',99),('d',99),('e',99)]
--
data ZipPad l r p q

instance (PP l a ~ x
        , PP r a ~ y
        , P l a
        , P r a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipPad l r p q) a where
  type PP (ZipPad l r p q) a = [(PP l a, PP r a)]
  eval _ opts a = do
    let msg0 = "ZipPad"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> pure e
          Right () -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              LT -> do
                ll <- eval (Proxy @l) opts a
                pure $ case getValueLR opts (msg0 <> " l failed") ll hhs of
                  Left e -> e
                  Right l ->
                    let d = zip (p ++ repeat l) q
                    in mkNode opts (PresentT d) [show01' opts (msg0 <> " Left pad") d "p=" p <> show1 opts " | q=" q] (hhs ++ [hh ll])
              GT -> do
                rr <- eval (Proxy @r) opts a
                pure $ case getValueLR opts (msg0 <> " r failed") rr hhs of
                  Left e -> e
                  Right r ->
                    let d =zip p (q ++ repeat r)
                    in mkNode opts (PresentT d) [show01' opts (msg0 <> " Right pad") d "p=" p <> show1 opts " | q=" q] (hhs ++ [hh rr])
              EQ ->
                let d = zip p q
                in pure $ mkNode opts (PresentT d) [show01' opts (msg0 <> " No pad") d "p=" p <> show1 opts " | q=" q] hhs


-- | zip two lists padding the left hand side if needed
--
-- >>> pl @(ZipL 99 '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (ZipL [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(ZipL 99 '[1,2] "abc") ()
-- Present [(1,'a'),(2,'b'),(99,'c')] (ZipL [(1,'a'),(2,'b'),(99,'c')] | p=[1,2] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(99,'c')]
--
-- >>> pl @(ZipL 99 '[1] "abc") ()
-- Present [(1,'a'),(99,'b'),(99,'c')] (ZipL [(1,'a'),(99,'b'),(99,'c')] | p=[1] | q="abc")
-- PresentT [(1,'a'),(99,'b'),(99,'c')]
--
-- >>> pl @(ZipL 99 '[1,2,3] "ab") ()
-- Error ZipL(3,2) rhs would be truncated
-- FailT "ZipL(3,2) rhs would be truncated"
--
data ZipL l p q
instance (PP l a ~ x
        , P l a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipL l p q) a where
  type PP (ZipL l p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipL"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> pure e
          Right () -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              GT -> let msg1 = msg0 ++ show lls
                    in pure $ mkNode opts (FailT (msg1 ++ " rhs would be truncated")) [msg1 <> "rhs would be truncated " <> show1 opts " | p=" p <> show1 opts " | q=" q] hhs
              _ -> do
                     ll <- eval (Proxy @l) opts a
                     pure $ case getValueLR opts (msg0 <> " l failed") ll hhs of
                             Left e -> e
                             Right l ->
                               let d = zip (p ++ repeat l) q
                               in mkNode opts (PresentT d) [show01' opts msg0 d "p=" p <> show1 opts " | q=" q] (hhs ++ [hh ll])

-- | zip two lists padding the right hand side if needed
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (ZipR [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "ab") ()
-- Present [(1,'a'),(2,'b'),(3,'Z')] (ZipR [(1,'a'),(2,'b'),(3,'Z')] | p=[1,2,3] | q="ab")
-- PresentT [(1,'a'),(2,'b'),(3,'Z')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2,3] "a") ()
-- Present [(1,'a'),(2,'Z'),(3,'Z')] (ZipR [(1,'a'),(2,'Z'),(3,'Z')] | p=[1,2,3] | q="a")
-- PresentT [(1,'a'),(2,'Z'),(3,'Z')]
--
-- >>> pl @(ZipR (Char1 "Z") '[1,2] "abc") ()
-- Error ZipR(2,3) rhs would be truncated
-- FailT "ZipR(2,3) rhs would be truncated"
--
data ZipR r p q
instance (PP r a ~ y
        , P r a
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipR r p q) a where
  type PP (ZipR r p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipR"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> pure e
          Right () -> do
            let lls = (length p,length q)
            case uncurry compare lls of
              LT -> let msg1 = msg0 ++ show lls
                    in pure $ mkNode opts (FailT (msg1 ++ " rhs would be truncated")) [msg1 <> "rhs would be truncated " <> show1 opts " | p=" p <> show1 opts " | q=" q] hhs
              _ -> do
                     rr <- eval (Proxy @r) opts a
                     pure $ case getValueLR opts (msg0 <> " l failed") rr hhs of
                             Left e -> e
                             Right r ->
                               let d = zip p (q ++ repeat r)
                               in mkNode opts (PresentT d) [show01' opts msg0 d "p=" p <> show1 opts " | q=" q] (hhs ++ [hh rr])

-- | zip two lists with the same length
--
-- >>> pl @(Zip '[1,2,3] "abc") ()
-- Present [(1,'a'),(2,'b'),(3,'c')] (Zip [(1,'a'),(2,'b'),(3,'c')] | p=[1,2,3] | q="abc")
-- PresentT [(1,'a'),(2,'b'),(3,'c')]
--
-- >>> pl @(Zip '[1,2,3] "ab") ()
-- Error Zip(3,2) length mismatch
-- FailT "Zip(3,2) length mismatch"
--
-- >>> pl @(Zip '[1,2] "abc") ()
-- Error Zip(2,3) length mismatch
-- FailT "Zip(2,3) length mismatch"
--
data Zip p q
instance (PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (Zip p q) a where
  type PP (Zip p q) a = [(ExtractAFromList (PP p a), ExtractAFromList (PP q a))]
  eval _ opts a = do
    let msg0 = "Zip"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> e
          Right () ->
            let lls = (length p, length q)
            in case uncurry compare lls of
                 EQ -> let d = zip p q
                       in mkNode opts (PresentT d) [show01' opts msg0 d "p=" p <> show1 opts " | q=" q] hhs
                 _ -> let msg1 = msg0 ++ show lls
                      in mkNode opts (FailT (msg1 <> " length mismatch")) [msg1 <> " length mismatch" ++ show1 opts " | p=" p <> show1 opts " | q=" q] hhs

-- | Luhn predicate check on last digit
--
-- >>> pz @(Luhn Id) [1,2,3,0]
-- True
-- TrueT
--
-- >>> pz @(Luhn Id) [1,2,3,4]
-- False
-- FalseT
--
-- >>> pz @(GuardSimple (Luhn Id)) [15,4,3,1,99]
-- Error (Luhn map=[90,2,3,8,6] sum=109 ret=9 | [15,4,3,1,99])
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
        in if ret == 0 then mkNodeB opts True [msg0 <> show0 opts " | " p] hhs
           else mkNodeB opts False [msg0 <> " map=" <> show ys <> " sum=" <> show z <> " ret=" <> show ret <> show1 opts " | " p] hhs

-- | Read a number using base 2 through a maximum of 36
--
-- >>> pz @(ReadBase Int 16 Id) "00feD"
-- Present 4077
-- PresentT 4077
--
-- >>> pz @(ReadBase Int 16 Id) "-ff"
-- Present -255
-- PresentT (-255)
--
-- >>> pz @(ReadBase Int 2 Id) "10010011"
-- Present 147
-- PresentT 147
--
-- >>> pz @(ReadBase Int 8 Id) "Abff"
-- Error invalid base 8
-- FailT "invalid base 8"
--
-- supports negative numbers unlike readInt
data ReadBase' t (n :: Nat) p

instance (Typeable (PP t x)
        , ZwischenT 2 36 n
        , Show (PP t x)
        , Num (PP t x)
        , KnownNat n
        , PP p x ~ String
        , P p x
        ) => P (ReadBase' t n p) x where
  type PP (ReadBase' t n p) x = PP t x
  eval _ opts x = do
    let n = nat @n
        xs = getValidBase n
        msg0 = "ReadBase(" <> t <> "," <> show n <> ")"
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (ff,p1) = case p of
                        '-':q -> (negate,q)
                        _ -> (id,p)
        in case readInt (fromIntegral n)
            ((`elem` xs) . toLower)
            (fromJust . (`elemIndex` xs) . toLower)
            p1 of
             [(b,"")] -> mkNode opts (PresentT (ff b)) [msg0 <> show0 opts " " (ff b) <> show1 opts " | " p] [hh pp]
             o -> mkNode opts (FailT ("invalid base " <> show n)) [msg0 <> " as=" <> p <> " err=" <> show o] [hh pp]

data ReadBase (t :: Type) (n :: Nat) p
type ReadBaseT (t :: Type) (n :: Nat) p = ReadBase' (Hole t) n p

instance P (ReadBaseT t n p) x => P (ReadBase t n p) x where
  type PP (ReadBase t n p) x = PP (ReadBaseT t n p) x
  eval _ = eval (Proxy @(ReadBaseT t n p))

data ReadBaseInt (n :: Nat) p
type ReadBaseIntT (n :: Nat) p = ReadBase' (Hole Int) n p

instance P (ReadBaseIntT n p) x => P (ReadBaseInt n p) x where
  type PP (ReadBaseInt n p) x = PP (ReadBaseIntT n p) x
  eval _ = eval (Proxy @(ReadBaseIntT n p))

getValidBase :: Int -> String
getValidBase n =
  let xs = ['0'..'9'] <> ['a'..'z']
      len = length xs
  in if n > len || n < 2 then errorInProgram $ "getValidBase: oops invalid base valid is 2 thru " ++ show len ++ " found " ++ show n
     else take n xs

-- | Display a number at base 2 to 36, similar to 'showIntAtBase' but supports signed numbers
--
-- >>> pz @(ShowBase 16 Id) 4077
-- Present "fed"
-- PresentT "fed"
--
-- >>> pz @(ShowBase 16 Id) (-255)
-- Present "-ff"
-- PresentT "-ff"
--
-- >>> pz @(ShowBase 2 Id) 147
-- Present "10010011"
-- PresentT "10010011"
--
-- >>> pz @(ShowBase 2 (Negate 147)) "whatever"
-- Present "-10010011"
-- PresentT "-10010011"
--
data ShowBase (n :: Nat) p

instance (PP p x ~ a
        , P p x
        , Show a
        , 2 GL.<= n
        , n GL.<= 36
        , KnownNat n
        , Integral a
        ) => P (ShowBase n p) x where
  type PP (ShowBase n p) x = String
  eval _ opts x = do
    let n = nat @n
        xs = getValidBase n
        msg0 = "ShowBase " <> show n
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (ff,a') = if p < 0 then (('-':), abs p) else (id,p)
            b = showIntAtBase (fromIntegral n) (xs !!) a' ""
        in mkNode opts (PresentT (ff b)) [msg0 <> showLit0 opts " " (ff b) <> show1 opts " | " p] []

-- | intercalate two lists
--
-- >>> pz @(Intercalate '["aB"] '["xxxx","yz","z","www","xyz"]) ()
-- Present ["xxxx","aB","yz","aB","z","aB","www","aB","xyz"]
-- PresentT ["xxxx","aB","yz","aB","z","aB","www","aB","xyz"]
--
-- >>> pz @(Intercalate '[W 99,Negate 98] Id) [1..5]
-- Present [1,99,-98,2,99,-98,3,99,-98,4,99,-98,5]
-- PresentT [1,99,-98,2,99,-98,3,99,-98,4,99,-98,5]
--
-- >>> pz @(Intercalate '[99,100] Id) [1..5]
-- Present [1,99,100,2,99,100,3,99,100,4,99,100,5]
--PresentT [1,99,100,2,99,100,3,99,100,4,99,100,5]
--
data Intercalate p q

instance (PP p x ~ [a]
        , PP q x ~ PP p x
        , P p x
        , P q x
        , Show a
      ) => P (Intercalate p q) x where
  type PP (Intercalate p q) x = PP p x
  eval _ opts x = do
    let msg0 = "Intercalate"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case chkSize opts msg0 p hhs <* chkSize opts msg0 q hhs of
          Left e -> e
          Right () ->
            let d = intercalate p (map (:[]) q)
            in mkNode opts (PresentT d) [show01 opts msg0 d p <> show1 opts " | " q] hhs

-- | uses PrintF to format output
--
-- >>> pz @(PrintF "value=%03d" Id) 12
-- Present "value=012"
-- PresentT "value=012"
--
-- >>> pz @(PrintF "%s" (Fst Id)) ("abc",'x')
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(PrintF "%d" (Fst Id)) ("abc",'x')
-- Error PrintF (IO e=printf: bad formatting char 'd')
-- FailT "PrintF (IO e=printf: bad formatting char 'd')"
--
data PrintF s p

instance (PrintfArg (PP p x)
        , Show (PP p x)
        , PP s x ~ String
        , P s x
        , P p x
        ) => P (PrintF s p) x where
  type PP (PrintF s p) x = String
  eval _ opts x = do
    let msg0 = "PrintF"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        let msg1 = msg0
        lr <- catchitNF @_ @E.SomeException (printf s p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg1 <> " (" <> e <> ")")) [msg1 <> show0 opts " " p <> " s=" <> s] [hh ss, hh pp]
          Right ret -> mkNode opts (PresentT ret) [msg1 <> " [" <> showLit0 opts "" ret <> "]" <> show1 opts " | p=" p <> showLit1 opts " | s=" s] [hh ss, hh pp]

type family GuardsT (ps :: [k]) where
  GuardsT '[] = '[]
  GuardsT (p ': ps) = Guard "fromGuardsT" p ': GuardsT ps

--type Guards' (ps :: [k]) = Para (GuardsT ps)

--type ToGuards (prt :: k) (os :: [k1]) = Proxy (Guards (ToGuardsT prt os))

type family ToGuardsT (prt :: k) (os :: [k1]) :: [(k,k1)] where
  ToGuardsT prt '[] = GL.TypeError ('GL.Text "ToGuardsT cannot be empty")
  ToGuardsT prt '[p] = '(prt,p) : '[]
  ToGuardsT prt (p ': ps) = '(prt,p) ': ToGuardsT prt ps

-- | runs values in parallel unlike 'Do' which is serial
--
-- >>> pz @(Para '[Id,Id + 1,Id * 4]) [10,20,30]
-- Present [10,21,120]
-- PresentT [10,21,120]
--
-- >>> pz @(Para '[Id,Id + 1,Id * 4]) [10,20,30,40]
-- Error Para: invalid length:expected 3 but found 4
-- FailT "Para: invalid length:expected 3 but found 4"
--
data ParaImpl (n :: Nat) (os :: [k])

data Para (ps :: [k])

-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance (GetLen ps, P (ParaImpl (LenT ps) ps) [a]) => P (Para ps) [a] where
  type PP (Para ps) [a] = PP (ParaImpl (LenT ps) ps) [a]
  eval _ opts as = do
    let msg0 = "Para"
        n = getLen @ps
    if n /= length as then
--       let msg1 = msg0 <> ": predicates(" <> show n <> ") /= data(" <> show (length as) <> ")"
       let msg1 = msg0 <> ": invalid length:expected " ++ show n ++ " but found " ++ show (length as)
       in pure $ mkNode opts (FailT msg1) [msg1] []
    else eval (Proxy @(ParaImpl (LenT ps) ps)) opts as

-- only allow non empty lists
instance GL.TypeError ('GL.Text "ParaImpl '[] invalid: requires at least one value in the list")
   => P (ParaImpl n ('[] :: [k])) [a] where
  type PP (ParaImpl n ('[] :: [k])) [a] = Void
  eval _ _ _ = errorInProgram "ParaImpl empty list"

instance (Show (PP p a)
        , KnownNat n
        , Show a
        , P p a
        ) => P (ParaImpl n '[p]) [a] where
  type PP (ParaImpl n '[p]) [a] = [PP p a]
  eval _ opts as' = do
    let msgbase0 = "Para"
        msgbase1 = msgbase0 <> "(" <> show n <> ")"
        n :: Int
        n = nat @n
    case as' of
      [a] -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msgbase1 pp [] of
          Left e -> e
          -- show1 opts " " [b]  fails but using 'b' is ok and (b : []) also works!
          -- GE.List problem
          Right b -> mkNode opts (PresentT [b]) [msgbase1 <> show0 opts " " (b : []) <> show1 opts " | " a] [hh pp]
      _ -> errorInProgram $ "ParaImpl base case should have exactly one element but found " ++ show as'

instance (KnownNat n
        , GetLen ps
        , P p a
        , P (ParaImpl n (p1 ': ps)) [a]
        , PP (ParaImpl n (p1 ': ps)) [a] ~ [PP p a]
        , Show a
        , Show (PP p a)
        )
     => P (ParaImpl n (p ': p1 ': ps)) [a] where
  type PP (ParaImpl n (p ': p1 ': ps)) [a] = [PP p a]
  eval _ opts as' = do
     let cpos = n-pos-1
         msgbase0 = msgbase2 <> "(" <> showIndex cpos <> " of " <> show n <> ")"
         msgbase1 = msgbase2 <> "(" <> showIndex cpos <> ")"
         msgbase2 = "Para"
         n = nat @n
         pos = 1 + getLen @ps -- cos p1!
     case as' of
       a:as -> do
         pp <- eval (Proxy @p) opts a
         case getValueLR opts msgbase0 pp [] of
           Left e -> pure e
           Right b -> do
                        qq <- eval (Proxy @(ParaImpl n (p1 ': ps))) opts as
                        pure $ case getValueLRHide opts (msgbase1 <> " rhs failed " <> show b) qq [hh pp] of
                          Left e -> e
                          Right bs -> mkNode opts (PresentT (b:bs)) [msgbase1 <> show0 opts " " (b:bs) <> show1 opts " | " as'] [hh pp, hh qq]
       _ -> errorInProgram $ "ParaImpl n+1 case has no data left"

-- | leverages 'Para' for repeating predicates (passthrough method)
--
-- >>> pz @(ParaN 4 (Succ Id)) [1..4]
-- Present [2,3,4,5]
-- PresentT [2,3,4,5]
--
-- >>> pz @(ParaN 4 (Succ Id)) "azwxm"
-- Error Para: invalid length:expected 4 but found 5
-- FailT "Para: invalid length:expected 4 but found 5"
--
-- >>> pz @(ParaN 4 (Succ Id)) "azwx"
-- Present "b{xy"
-- PresentT "b{xy"
--
data ParaN (n :: Nat) p

instance ( P (ParaImpl (LenT (RepeatT n p)) (RepeatT n p)) [a]
         , GetLen (RepeatT n p)
         ) => P (ParaN n p) [a] where
  type PP (ParaN n p) [a] = PP (Para (RepeatT n p)) [a]
  eval _ opts as =
    eval (Proxy @(Para (RepeatT n p))) opts as

-- | tries each predicate ps and on the first match runs the corresponding qs but if there is no match on ps then runs the fail case e
--
-- >>> pz @(Case (FailS "asdf" >> Snd Id >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 50
-- Present "50 is same50"
-- PresentT "50 is same50"
--
-- >>> pz @(Case (FailS "asdf" >> Snd Id >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 9
-- Present "9 is lt10"
-- PresentT "9 is lt10"
--
-- >>> pz @(Case (FailS "asdf" >> Snd Id >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 3
-- Present "3 is lt4"
-- PresentT "3 is lt4"
--
-- >>> pz @(Case (FailS "asdf" >> Snd Id >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[PrintF "%d is lt4" Id, PrintF "%d is lt10" Id, PrintF "%d is same50" Id] Id) 99
-- Error asdf
-- FailT "asdf"
--
data CaseImpl (n :: Nat) (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)
-- ps = conditions
-- qs = what to do [one to one
-- r = the value
-- e = otherwise  -- leave til later
data Case (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)
data Case' (ps :: [k]) (qs :: [k1]) (r :: k2)
data Case'' s (ps :: [k]) (qs :: [k1]) (r :: k2)

type CaseT' (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (Snd Id >> Failp "Case:no match") ps qs r
type CaseT'' s (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (FailCaseT s) ps qs r -- eg s= PrintF "%s" (ShowP Id)

instance P (CaseT'' s ps qs r) x => P (Case'' s ps qs r) x where
  type PP (Case'' s ps qs r) x = PP (CaseT'' s ps qs r) x
  eval _ = eval (Proxy @(CaseT'' s ps qs r))

instance P (CaseT' ps qs r) x => P (Case' ps qs r) x where
  type PP (Case' ps qs r) x = PP (CaseT' ps qs r) x
  eval _ = eval (Proxy @(CaseT' ps qs r))

type FailCaseT p = Fail (Snd Id >> Unproxy) (Fst Id >> p)

type CaseImplT e ps qs r = CaseImpl (LenT ps) e ps qs r

-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance (FailUnlessT (LenT ps DE.== LenT qs)
                  ('GL.Text "lengths are not the same "
                   ':<>: 'GL.ShowType (LenT ps)
                   ':<>: 'GL.Text " vs "
                   ':<>: 'GL.ShowType (LenT qs))
        , P (CaseImplT e ps qs r) x
        ) => P (Case e ps qs r) x where
  type PP (Case e ps qs r) x = PP (CaseImplT e ps qs r) x
  eval _ = eval (Proxy @(CaseImplT e ps qs r))

-- only allow non empty lists!
instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lhs requires at least one value in the list"))
   => P (CaseImpl n e ('[] :: [k]) (q ': qs) r) x where
  type PP (CaseImpl n e ('[] :: [k]) (q ': qs) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl lhs empty"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: rhs requires at least one value in the list"))
   => P (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl rhs empty"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lists are both empty"))
   => P (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x = Void
  eval _ _ _ = errorInProgram "CaseImpl both lists empty"

instance (P r x
        , P q (PP r x)
        , Show (PP q (PP r x))
        , P p (PP r x)
        , PP p (PP r x) ~ Bool
        , KnownNat n
        , Show (PP r x)
        , P e (PP r x, Proxy (PP q (PP r x)))
        , PP e (PP r x, Proxy (PP q (PP r x))) ~ PP q (PP r x)
        ) => P (CaseImpl n e '[p] '[q] r) x where
  type PP (CaseImpl n e '[p] '[q] r) x = PP q (PP r x)
  eval _ opts z = do
    let msgbase0 = "Case" <> "(" <> show n <> ")"
        n :: Int = nat @n
    rr <- eval (Proxy @r) opts z
    case getValueLR opts msgbase0 rr [] of
      Left e -> pure e
      Right a -> do
        pp <- evalBool (Proxy @p) opts a
        case getValueLR opts msgbase0 pp [hh rr] of
          Left e -> pure e
          Right True -> do
            qq <- eval (Proxy @q) opts a
            pure $ case getValueLR opts msgbase0 qq [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [show01 opts msgbase0 b a] (hh rr : hh pp : if isVerbose opts then [hh qq] else [])
          Right False -> do
            ee <- eval (Proxy @e) opts (a, Proxy @(PP q (PP r x)))
            pure $ case getValueLR opts (msgbase0 <> "  otherwise failed") ee [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [show01 opts msgbase0 b a] [hh rr, hh pp, hh ee]

instance (KnownNat n
        , GetLen ps
        , P r x
        , P p (PP r x)
        , P q (PP r x)
        , PP p (PP r x) ~ Bool
        , Show (PP q (PP r x))
        , Show (PP r x)
        , P (CaseImpl n e (p1 ': ps) (q1 ': qs) r) x
        , PP (CaseImpl n e (p1 ': ps) (q1 ': qs) r) x ~ PP q (PP r x)
        )
     => P (CaseImpl n e (p ': p1 ': ps) (q ': q1 ': qs) r) x where
  type PP (CaseImpl n e (p ': p1 ': ps) (q ': q1 ': qs) r) x = PP q (PP r x)
  eval _ opts z = do
    let cpos = n-pos-1
        msgbase0 = msgbase2 <> "(" <> showIndex cpos <> " of " <> show n <> ")"
        msgbase1 = msgbase2 <> "(" <> showIndex cpos <> ")"
        msgbase2 = "Case"
        n = nat @n
        pos = 1 + getLen @ps -- cos p1!
    rr <- eval (Proxy @r) opts z
    case getValueLR opts msgbase0 rr [] of
      Left e -> pure e
      Right a -> do
        pp <- evalBool (Proxy @p) opts a
        case getValueLR opts msgbase0 pp [hh rr] of
          Left e -> pure e
          Right True -> do
            qq <- eval (Proxy @q) opts a
            pure $ case getValueLR opts msgbase0 qq [hh pp, hh rr] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [show01 opts msgbase0 b a] (hh rr : hh pp : if isVerbose opts then [hh qq] else [])
          Right False -> do
            ww <- eval (Proxy @(CaseImpl n e (p1 ': ps) (q1 ': qs) r)) opts z
            pure $ case getValueLR opts (msgbase1 <> " failed rhs") ww [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [show01 opts msgbase1 b a] [hh rr, hh pp, hh ww]

-- | similar to 'sequenceA'
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30]
-- Present Just [10,20,30]
-- PresentT (Just [10,20,30])
--
-- >>> pz @Sequence [Just 10, Just 20, Just 30, Nothing, Just 40]
-- Present Nothing
-- PresentT Nothing
--
data Sequence
type TraverseT p q = Map p q >> Sequence

data Traverse p q

instance P (TraverseT p q) x => P (Traverse p q) x where
  type PP (Traverse p q) x = PP (TraverseT p q) x
  eval _ = eval (Proxy @(TraverseT p q))

instance (Show (f (t a))
        , Show (t (f a))
        , Traversable t
        , Applicative f
        ) => P Sequence (t (f a)) where
  type PP Sequence (t (f a)) = f (t a)
  eval _ opts tfa =
     let d = sequenceA tfa
     in pure $ mkNode opts (PresentT d) ["Sequence" <> show0 opts " " d <> show1 opts " | " tfa] []

-- | run the expression \'p\' but remove the subtrees
data Hide p
-- type H p = Hide p -- doesnt work with %   -- unsaturated!

instance P p x => P (Hide p) x where
  type PP (Hide p) x = PP p x
  eval _ opts x = do
      tt <- eval (Proxy @(Msg "!" p)) opts x
      pure $ tt & tForest .~ []

-- | similar to 'readFile'
--
-- >>> pz @(ReadFile ".ghci" >> 'Just Id >> Len > 0) ()
-- True
-- TrueT
--
-- >>> pz @(FileExists "xyzzy") ()
-- False
-- FalseT
--
data ReadFile p

data FileExists p
type FileExistsT p = ReadFile p >> IsJust

instance P (FileExistsT p) x => P (FileExists p) x where
  type PP (FileExists p) x = PP (FileExistsT p) x
  eval _ = eval (Proxy @(FileExistsT p))

instance (PP p x ~ String, P p x) => P (ReadFile p) x where
  type PP (ReadFile p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadFile"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT (msg1 <> " must run in IO")) [msg1 <> " must run in IO"] [hh pp]
          Just Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " does not exist"] [hh pp]
          Just (Just b) -> mkNode opts (PresentT (Just b)) [msg1 <> " len=" <> show (length b) <> showLit0 opts " Just " b] [hh pp]

-- | does the directory exists
--
-- >>> pz @(DirExists ".") ()
-- True
-- TrueT
--
data ReadDir p
data DirExists p
type DirExistsT p = ReadDir p >> IsJust

instance P (DirExistsT p) x => P (DirExists p) x where
  type PP (DirExists p) x = PP (DirExistsT p) x
  eval _ = eval (Proxy @(DirExistsT p))


instance (PP p x ~ String, P p x) => P (ReadDir p) x where
  type PP (ReadDir p) x = Maybe [FilePath]
  eval _ opts x = do
    let msg0 = "ReadDir"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ do
                b <- doesDirectoryExist p
                if b then Just <$> listDirectory p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT (msg1 <> " must run in IO")) [msg1 <> " must run in IO"] []
          Just Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " does not exist"] []
          Just (Just b) -> mkNode opts (PresentT (Just b)) [msg1 <> " len=" <> show (length b) <> show0 opts " Just " b] []

-- | does the directory exists
--
-- >>> pz @(ReadEnv "PATH" >> 'Just Id >> 'True) ()
-- True
-- TrueT
--
data ReadEnv p

instance (PP p x ~ String, P p x) => P (ReadEnv p) x where
  type PP (ReadEnv p) x = Maybe String
  eval _ opts x = do
    let msg0 = "ReadEnv"
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let msg1 = msg0 <> "[" <> p <> "]"
        mb <- runIO $ lookupEnv p
        pure $ case mb of
          Nothing -> mkNode opts (FailT (msg1 <> " must run in IO")) [msg1 <> " must run in IO"] []
          Just Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " does not exist"] []
          Just (Just v) -> mkNode opts (PresentT (Just v)) [msg1 <> showLit0 opts " " v] []

data ReadEnvAll

instance P ReadEnvAll a where
  type PP ReadEnvAll a = [(String,String)]
  eval _ opts _ = do
    let msg0 = "ReadEnvAll"
    mb <- runIO $ getEnvironment
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) [msg0 <> " must run in IO"] []
      Just v -> mkNode opts (PresentT v) [msg0 <> " count=" <> show (length v)] []

data TimeUtc

instance P TimeUtc a where
  type PP TimeUtc a = UTCTime
  eval _ opts _a = do
    let msg0 = "TimeUtc"
    mb <- runIO $ getCurrentTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) [msg0 <> " must run in IO"] []
      Just v -> mkNode opts (PresentT v) [msg0 <> show0 opts " " v] []

data TimeZt

instance P TimeZt a where
  type PP TimeZt a = ZonedTime
  eval _ opts _a = do
    let msg0 = "TimeZt"
    mb <- runIO $ getZonedTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) [msg0 <> " must run in IO"] []
      Just v -> mkNode opts (PresentT v) [msg0 <> show0 opts " " v] []

data FHandle s = FStdout | FStderr | FOther s WFMode deriving Show

class GetFHandle (x :: FHandle Symbol) where getFHandle :: FHandle String
instance GetFHandle 'FStdout where getFHandle = FStdout
instance GetFHandle 'FStderr where getFHandle = FStderr
instance (GetMode w, KnownSymbol s) => GetFHandle ('FOther s w) where getFHandle = FOther (symb @s) (getMode @w)

data WFMode = WFAppend | WFWrite | WFWriteForce deriving (Show,Eq)

class GetMode (x :: WFMode) where getMode :: WFMode
instance GetMode 'WFAppend where getMode = WFAppend
instance GetMode 'WFWriteForce where getMode = WFWriteForce
instance GetMode 'WFWrite where getMode = WFWrite

data WriteFileImpl (hh :: FHandle Symbol) p

data AppendFile (s :: Symbol) p
type AppendFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFAppend) p

instance P (AppendFileT s p) x => P (AppendFile s p) x where
  type PP (AppendFile s p) x = PP (AppendFileT s p) x
  eval _ = eval (Proxy @(AppendFileT s p))


data WriteFile' (s :: Symbol) p
type WriteFileT' (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWriteForce) p

instance P (WriteFileT' s p) x => P (WriteFile' s p) x where
  type PP (WriteFile' s p) x = PP (WriteFileT' s p) x
  eval _ = eval (Proxy @(WriteFileT' s p))

data WriteFile (s :: Symbol) p
type WriteFileT (s :: Symbol) p = WriteFileImpl ('FOther s 'WFWrite) p

instance P (WriteFileT s p) x => P (WriteFile s p) x where
  type PP (WriteFile s p) x = PP (WriteFileT s p) x
  eval _ = eval (Proxy @(WriteFileT s p))


data Stdout p
type StdoutT p = WriteFileImpl 'FStdout p

instance P (StdoutT p) x => P (Stdout p) x where
  type PP (Stdout p) x = PP (StdoutT p) x
  eval _ = eval (Proxy @(StdoutT p))

data Stderr p
type StderrT p = WriteFileImpl 'FStderr p

instance P (StderrT p) x => P (Stderr p) x where
  type PP (Stderr p) x = PP (StderrT p) x
  eval _ = eval (Proxy @(StderrT p))

instance (GetFHandle fh
        , P p a
        , PP p a ~ String
        ) => P (WriteFileImpl fh p) a where
  type PP (WriteFileImpl fh p) a = ()
  eval _ opts a = do
    let fh = getFHandle @fh
        msg0 = case fh of
                      FStdout -> "Stdout"
                      FStderr -> "Stderr"
                      FOther s w -> (<>("[" <> s <> "]")) $ case w of
                         WFAppend -> "AppendFile"
                         WFWrite -> "WriteFile"
                         WFWriteForce -> "WriteFile'"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right ss -> do
          mb <- runIO $ do
                          case fh of
                            FStdout -> fmap (left show) $ E.try @E.SomeException $ hPutStr stdout ss
                            FStderr -> fmap (left show) $ E.try @E.SomeException $ hPutStr stderr ss
                            FOther s w -> do
                               b <- doesFileExist s
                               if b && w == WFWrite then pure $ Left $ "file [" <> s <> "] already exists"
                               else do
                                      let md = case w of
                                             WFAppend -> AppendMode
                                             _ -> WriteMode
                                      fmap (left show) $ E.try @E.SomeException $ withFile s md (flip hPutStr ss)
          pure $ case mb of
            Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) [msg0 <> " must run in IO"] [hh pp]
            Just (Left e) -> mkNode opts (FailT e) [msg0 <> " " <> e] [hh pp]
            Just (Right ()) -> mkNode opts (PresentT ()) [msg0] [hh pp]

data Stdin

instance P Stdin a where
  type PP Stdin a = String
  eval _ opts _a = do
    let msg0 = "Stdin"
    mb <- runIO $ do
                      lr <- E.try $ hGetContents stdin
                      pure $ case lr of
                        Left (e :: E.SomeException) -> Left $ show e
                        Right ss -> Right ss
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg0 <> " must run in IO")) [msg0 <> " must run in IO"] []
      Just (Left e) -> mkNode opts (FailT e) [msg0 <> " " <> e] []
      Just (Right ss) -> mkNode opts (PresentT ss) [msg0 <> "[" <> showLit1 opts "" ss <> "]"] []

--type Just' = JustFail "expected Just" Id
--type Nothing' = Guard "expected Nothing" IsNothing

-- | similar to 'isInfixOf' 'isPrefixOf' 'isSuffixOf' for strings only.
--
-- The \'I\' suffixed versions work are case insensitive.
--
-- >>> pz @(IsInfixI "abc" "axAbCd") ()
-- True
-- TrueT
--
-- >>> pz @(IsPrefixI "abc" "aBcbCd") ()
-- True
-- TrueT
--
-- >>> pz @(IsPrefix "abc" "aBcbCd") ()
-- False
-- FalseT
--
-- >>> pz @(IsSuffix "bCd" "aBcbCd") ()
-- True
-- TrueT
--
data IsFixImpl (cmp :: Ordering) (ignore :: Bool) p q


data IsPrefix p q
type IsPrefixT p q = IsFixImpl 'LT 'False p q

instance P (IsPrefixT p q) x => P (IsPrefix p q) x where
  type PP (IsPrefix p q) x = PP (IsPrefixT p q) x
  eval _ = eval (Proxy @(IsPrefixT p q))

data IsInfix p q
type IsInfixT p q = IsFixImpl 'EQ 'False p q

instance P (IsInfixT p q) x => P (IsInfix p q) x where
  type PP (IsInfix p q) x = PP (IsInfixT p q) x
  eval _ = eval (Proxy @(IsInfixT p q))

data IsSuffix p q
type IsSuffixT p q = IsFixImpl 'GT 'False p q

instance P (IsSuffixT p q) x => P (IsSuffix p q) x where
  type PP (IsSuffix p q) x = PP (IsSuffixT p q) x
  eval _ = eval (Proxy @(IsSuffixT p q))

data IsPrefixI p q
type IsPrefixIT p q = IsFixImpl 'LT 'True p q

instance P (IsPrefixIT p q) x => P (IsPrefixI p q) x where
  type PP (IsPrefixI p q) x = PP (IsPrefixIT p q) x
  eval _ = eval (Proxy @(IsPrefixIT p q))

data IsInfixI p q
type IsInfixIT p q = IsFixImpl 'EQ 'True p q

instance P (IsInfixIT p q) x => P (IsInfixI p q) x where
  type PP (IsInfixI p q) x = PP (IsInfixIT p q) x
  eval _ = eval (Proxy @(IsInfixIT p q))

data IsSuffixI p q
type IsSuffixIT p q = IsFixImpl 'GT 'True p q

instance P (IsSuffixIT p q) x => P (IsSuffixI p q) x where
  type PP (IsSuffixI p q) x = PP (IsSuffixIT p q) x
  eval _ = eval (Proxy @(IsSuffixIT p q))

instance (GetBool ignore
        , P p x
        , P q x
        , PP p x ~ String
        , PP q x ~ String
        , GetOrdering cmp
        ) => P (IsFixImpl cmp ignore p q) x where
  type PP (IsFixImpl cmp ignore p q) x = Bool
  eval _ opts x = do
    let cmp = getOrdering @cmp
        ignore = getBool @ignore
        lwr = if ignore then map toLower else id
        (ff,msg0) = case cmp of
                    LT -> (isPrefixOf, "IsPrefix")
                    EQ -> (isInfixOf, "IsInfix")
                    GT -> (isSuffixOf, "IsSuffix")
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
        Left e -> pure e
        Right s0 -> do
          let msg1 = msg0 <> (if ignore then "I" else "") <> "(" <> s0 <> ")"
          qq <- eval (Proxy @q) opts x
          pure $ case getValueLR opts (msg1 <> " q failed") qq [hh pp] of
            Left e -> e
            Right s1 -> mkNodeB opts (on ff lwr s0 s1) [msg1 <> showLit0 opts " " s1] [hh pp, hh qq]

-- | similar to 'SG.<>'
--
-- >>> pz @(Fst Id <> Snd Id) ("abc","def")
-- Present "abcdef"
-- PresentT "abcdef"
--
-- >>> pz @("abcd" <> "ef" <> Id) "ghi"
-- Present "abcdefghi"
-- PresentT "abcdefghi"
--
-- >>> pz @("abcd" <> "ef" <> Id) "ghi"
-- Present "abcdefghi"
-- PresentT "abcdefghi"
--
-- >>> pz @(Wrap (SG.Sum _) Id <> FromInteger _ 10) 13
-- Present Sum {getSum = 23}
-- PresentT (Sum {getSum = 23})
--
-- >>> pz @(Wrap (SG.Product _) Id <> FromInteger _ 10) 13
-- Present Product {getProduct = 130}
-- PresentT (Product {getProduct = 130})
--
-- >>> pz @('(FromInteger _ 10,"def") <> Id) (SG.Sum 12, "_XYZ")
-- Present (Sum {getSum = 22},"def_XYZ")
-- PresentT (Sum {getSum = 22},"def_XYZ")
--
-- >>> pz @(Sapa' (SG.Max _)) (10,12)
-- Present Max {getMax = 12}
-- PresentT (Max {getMax = 12})
--
-- >>> pz @(Sapa' (SG.Sum _)) (10,12)
-- Present Sum {getSum = 22}
-- PresentT (Sum {getSum = 22})
--
data p <> q
infixr 6 <>

instance (Semigroup (PP p x)
        , PP p x ~ PP q x
        , P p x
        , Show (PP q x)
        ,P q x
        ) => P (p <> q) x where
  type PP (p <> q) x = PP p x
  eval _ opts x = do
    let msg0 = "<>"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <> q
        in mkNode opts (PresentT d) [show p <> " <> " <> show q <> " = " <> show d] [hh pp, hh qq]

data Sapa' (t :: Type)
type SapaT' (t :: Type) = Wrap t (Fst Id) <> Wrap t (Snd Id)

instance P (SapaT' t) x => P (Sapa' t) x where
  type PP (Sapa' t) x = PP (SapaT' t) x
  eval _ = eval (Proxy @(SapaT' t))

data Sapa
type SapaT = Fst Id <> Snd Id

instance P SapaT x => P Sapa x where
  type PP Sapa x = PP SapaT x
  eval _ = eval (Proxy @SapaT)

-- | uses inductive tuples to replace variable arguments
--
class PrintC x where
  prtC :: (PrintfArg a, PrintfType r) => String -> (a,x) -> r
instance PrintC () where
  prtC s (a,()) = printf s a
instance (PrintfArg a, PrintC rs) => PrintC (a,rs) where
  prtC s (a,rs) = prtC s rs a

-- | print for flat n-tuples
--
-- >>> pl @(PrintT "%d %s %s %s" '(Fst Id, Snd Id, Snd Id,Snd Id)) (10,"Asdf")
-- Present "10 Asdf Asdf Asdf" (PrintT [10 Asdf Asdf Asdf] | s=%d %s %s %s)
-- PresentT "10 Asdf Asdf Asdf"
--
-- >>> pl @(PrintT "%c %d %s" Id) ('x', 10,"Asdf")
-- Present "x 10 Asdf" (PrintT [x 10 Asdf] | s=%c %d %s)
-- PresentT "x 10 Asdf"
--
-- >>> pz @(PrintT "fst=%s snd=%03d" Id) ("ab",123)
-- Present "fst=ab snd=123"
-- PresentT "fst=ab snd=123"
--
-- >>> pz @(PrintT "fst=%s snd=%03d thd=%s" Id) ("ab",123,"xx")
-- Present "fst=ab snd=123 thd=xx"
-- PresentT "fst=ab snd=123 thd=xx"
--
-- >>> pl @(PrintT "%s %d %c %s" '(W "xyz", Fst Id, Snd Id, Thd Id)) (123,'x',"ab")
-- Present "xyz 123 x ab" (PrintT [xyz 123 x ab] | s=%s %d %c %s)
-- PresentT "xyz 123 x ab"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x')
-- Error PrintT(IO e=printf: argument list ended prematurely)
-- FailT "PrintT(IO e=printf: argument list ended prematurely)"
--
-- >>> pl @(PrintT "%d %c %s" Id) (123,'x',"abc",11)
-- Error PrintT(IO e=printf: formatting string ended prematurely)
-- FailT "PrintT(IO e=printf: formatting string ended prematurely)"
--
data PrintT s p
instance (PrintC bs
        , (b,bs) ~ InductTupleP y
        , InductTupleC y
        , PrintfArg b
        , PP s x ~ String
        , PP p x ~ y
        , P s x
        , P p x
        , CheckT (PP p x) ~ 'True
        ) => P (PrintT s p) x where
  type PP (PrintT s p) x = String
  eval _ opts x = do
    let msg0 = "PrintT"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,y,ss,pp) -> do
        let msg1 = msg0
            hhs = [hh ss, hh pp]
        lr <- catchitNF @_ @E.SomeException (prtC @bs s (inductTupleC y))
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg1 <> "(" <> e <> ")")) [msg1 <> " s=" <> s] hhs
          Right ret -> mkNode opts (PresentT ret) [msg1 <> " [" <> showLit0 opts "" ret <> "]" <> showLit0 opts " | s=" s] hhs

-- | print for lists  -- if you can use 'PrintT'
--
-- >>> pl @(PrintL 4 "%s %s %s %s" '[W "xyz", ShowP (Fst Id), ShowP (Snd Id), Thd Id]) (123,'x',"ab")
-- Present "xyz 123 'x' ab" (PrintL(4) [xyz 123 'x' ab] | s=%s %s %s %s)
-- PresentT "xyz 123 'x' ab"
--
-- >>> pl @(PrintL 3 "first=%d second=%d third=%d" Id) [10,11,12]
-- Present "first=10 second=11 third=12" (PrintL(3) [first=10 second=11 third=12] | s=first=%d second=%d third=%d)
-- PresentT "first=10 second=11 third=12"
--
-- >>> pl @(PrintL 2 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error PrintL(2) arg count=3
-- FailT "PrintL(2) arg count=3"
--
-- >>> pl @(PrintL 4 "first=%d second=%d third=%d" Id) [10,11,12]
-- Error PrintL(4) arg count=3
-- FailT "PrintL(4) arg count=3"
--
data PrintL (n :: Nat) s p

instance (KnownNat n
        , PrintC bs
        , (b,bs) ~ InductListP n a
        , InductListC n a
        , PrintfArg b
        , PP s x ~ String
        , PP p x ~ [a]
        , P s x
        , P p x
        ) => P (PrintL n s p) x where
  type PP (PrintL n s p) x = String
  eval _ opts x = do
    let msg0 = "PrintL(" ++ show n ++ ")"
        n = nat @n
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x []
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        let hhs = [hh ss, hh pp]
        if length p /= n then pure $ mkNode opts (FailT (msg0 <> " arg count=" ++ show (length p))) [msg0 <> " wrong length " ++ show (length p)] hhs
        else do
          lr <- catchitNF @_ @E.SomeException (prtC @bs s (inductListC @n @a p))
          pure $ case lr of
            Left e -> mkNode opts (FailT (msg0 <> "(" <> e <> ")")) [msg0 <> " s=" <> s] hhs
            Right ret -> mkNode opts (PresentT ret) [msg0 <> " [" <> showLit0 opts "" ret <> "]" <> showLit0 opts " | s=" s] hhs

type family CheckT (tp :: Type) :: Bool where
  CheckT () = GL.TypeError ('GL.Text "Printfn: inductive tuple cannot be empty")
  CheckT o = 'True

type family ApplyConstT (ta :: Type) (b :: Type) :: Type where
--type family ApplyConstT ta b where -- less restrictive so allows ('Just Int) Bool through!
  ApplyConstT (t a) b = t b
  ApplyConstT ta b = GL.TypeError (
       'GL.Text "ApplyConstT: (t a) b but found something else"
       ':$$: 'GL.Text "t a = "
       ':<>: 'GL.ShowType ta
       ':$$: 'GL.Text "b = "
       ':<>: 'GL.ShowType b)

-- | similar to 'Control.Applicative.<$'
--
-- >>> pz @(Fst Id <$ Snd Id) ("abc",Just 20)
-- Present Just "abc"
-- PresentT (Just "abc")
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
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " p] [hh pp, hh qq]

data p <* q
infixl 4 <*

-- | similar to 'Control.Applicative.<*'
--
-- >>> pz @(Fst Id <* Snd Id) (Just "abc",Just 20)
-- Present Just "abc"
-- PresentT (Just "abc")
--
type ArrowRT p q = q <* p
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
        in mkNode opts (PresentT d) [show01' opts msg0 p "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]

-- | similar to 'Control.Applicative.<|>'
--
-- >>> pz @(Fst Id <|> Snd Id) (Nothing,Just 20)
-- Present Just 20
-- PresentT (Just 20)
--
-- >>> pz @(Fst Id <|> Snd Id) (Just 10,Just 20)
-- Present Just 10
-- PresentT (Just 10)
--
-- >>> pz @(Fst Id <|> Snd Id) (Nothing,Nothing)
-- Present Nothing
-- PresentT Nothing
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
        in mkNode opts (PresentT d) [show01' opts msg0 d "p=" p <> show1 opts " | q=" q] [hh pp, hh qq]


-- | similar to 'Control.Comonad.extract'
--
-- >>> pz @Extract (Nothing,Just 20)
-- Present Just 20
-- PresentT (Just 20)
--
-- >>> pz @Extract (Identity 20)
-- Present 20
-- PresentT 20
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d ta] []

-- | similar to 'Control.Comonad.duplicate'
--
-- >>> pz @Duplicate (20,"abc")
-- Present (20,(20,"abc"))
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d ta] []

-- | similar to 'Control.Monad.join'
--
-- >>> pz @Join  (Just (Just 20))
-- Present Just 20
-- PresentT (Just 20)
--
-- >>> pz @Join  ["ab","cd","","ef"]
-- Present "abcdef"
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
    in pure $ mkNode opts (PresentT d) [show01 opts msg0 d tta] []

-- | function application for expressions: similar to 'GHC.Base.$'
--
-- pz @(Fst Id $$ Snd Id) ((*16),4)
-- Present 64
-- PresentT 64
--
-- pz @(Id $$ "def") ("abc"<>)
-- Present "abcdef"
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
        in mkNode opts (PresentT d) ["fn $$ " <> show q <> " = " <> show d] [hh pp, hh qq]

-- reify this so we can combine (type synonyms dont work as well)

-- | flipped function application for expressions: similar to 'Control.Lens.&'
--
-- pz @(Snd Id $& Fst Id) ((*16),4)
-- Present 64
-- PresentT 64
--
-- pz @("def" $& Id) ("abc"<>)
-- Present "abcdef"
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
        in mkNode opts (PresentT d) ["fn $& " <> show q <> " = " <> show d] [hh pp, hh qq]

type family FnT ab :: Type where
  FnT (a -> b) = b
  FnT ab = GL.TypeError (
      'GL.Text "FnT: expected Type -> Type but found a simple Type?"
      ':$$: 'GL.Text "ab = "
      ':<>: 'GL.ShowType ab)

-- | similar to 'T.strip' 'T.stripStart' 'T.stripEnd'
--
-- >>> pz @(TrimBoth (Snd Id)) (20," abc   " :: String)
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(TrimBoth (Snd Id)) (20,T.pack " abc   ")
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(TrimL (Snd Id)) (20," abc   ")
-- Present "abc   "
-- PresentT "abc   "
--
-- >>> pz @(TrimR (Snd Id)) (20," abc   ")
-- Present " abc"
-- PresentT " abc"
--
-- >>> pz @(TrimR "  abc ") ()
-- Present "  abc"
-- PresentT "  abc"
--
-- >>> pz @(TrimR "") ()
-- Present ""
-- PresentT ""
--
-- >>> pz @(TrimBoth "         ") ()
-- Present ""
-- PresentT ""
--
-- >>> pz @(TrimBoth "") ()
-- Present ""
-- PresentT ""
--
data TrimImpl (left :: Bool) (right :: Bool) p

instance (FailUnlessT (OrT l r)
           ('GL.Text "TrimImpl: left and right cannot both be False")
        , GetBool l
        , GetBool r
        , TL.IsText (PP p x)
        , P p x
        ) => P (TrimImpl l r p) x where
  type PP (TrimImpl l r p) x = PP p x
  eval _ opts x = do
    let msg0 = "Trim" ++ (if l && r then "Both" else if l then "L" else "R")
        l = getBool @l
        r = getBool @r
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right (view TL.unpacked -> p) ->
        let fl = if l then dropWhile isSpace else id
            fr = if r then dropWhileEnd isSpace else id
            b =  (fl . fr) p
        in mkNode opts (PresentT (b ^. TL.packed)) [msg0 <> showLit0 opts "" b <> showLit1 opts " | " p] [hh pp]

data TrimL p
type TrimLT p = TrimImpl 'True 'False p

instance P (TrimLT p) x => P (TrimL p) x where
  type PP (TrimL p) x = PP (TrimLT p) x
  eval _ = eval (Proxy @(TrimLT p))

data TrimR p
type TrimRT p = TrimImpl 'False 'True p

instance P (TrimRT p) x => P (TrimR p) x where
  type PP (TrimR p) x = PP (TrimRT p) x
  eval _ = eval (Proxy @(TrimRT p))

data TrimBoth p
type TrimBothT p = TrimImpl 'True 'True p

instance P (TrimBothT p) x => P (TrimBoth p) x where
  type PP (TrimBoth p) x = PP (TrimBothT p) x
  eval _ = eval (Proxy @(TrimBothT p))

-- | similar to 'T.stripLeft' 'T.stripRight'
--
-- >>> pz @(StripL "xyz" Id) ("xyzHello" :: String)
-- Present Just "Hello"
-- PresentT (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) (T.pack "xyzHello")
-- Present Just "Hello"
-- PresentT (Just "Hello")
--
-- >>> pz @(StripL "xyz" Id) "xywHello"
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @(StripR "xyz" Id) "Hello xyz"
-- Present Just "Hello "
-- PresentT (Just "Hello ")
--
-- >>> pz @(StripR "xyz" Id) "xyzHelloxyw"
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @(StripR "xyz" Id) ""
-- Present Nothing
-- PresentT Nothing
--
-- >>> pz @(StripR "xyz" "xyz") ()
-- Present Just ""
-- PresentT (Just "")
--
data StripImpl(left :: Bool) p q

instance (GetBool l
        , PP p x ~ String
        , P p x
        , TL.IsText (PP q x)
        , P q x
        ) => P (StripImpl l p q) x where
  type PP (StripImpl l p q) x = Maybe (PP q x)
  eval _ opts x = do
    let msg0 = "Strip" ++ if l then "L" else "R"
        l = getBool @l
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,view TL.unpacked -> q,pp,qq) ->
        let b = if l then
                  let (before,after) = splitAt (length p) q
                  in if before == p then Just after else Nothing
                else
                  let (before,after) = splitAt (length q - length p) q
                  in if after == p then Just before else Nothing
        in mkNode opts (PresentT (fmap (view TL.packed) b)) [msg0 <> show0 opts "" b <> showLit1 opts " | p=" p <> showLit1 opts " | q=" q] [hh pp, hh qq]

data StripL p q
type StripLT p q = StripImpl 'True p q

instance P (StripLT p q) x => P (StripL p q) x where
  type PP (StripL p q) x = PP (StripLT p q) x
  eval _ = eval (Proxy @(StripLT p q))

data StripR p q
type StripRT p q = StripImpl 'False p q

instance P (StripRT p q) x => P (StripR p q) x where
  type PP (StripR p q) x = PP (StripRT p q) x
  eval _ = eval (Proxy @(StripRT p q))

-- | creates a promoted list of predicates and then evaluates them into a list. see PP instance for '[k]
--
-- >>> pz @(Repeat 4 (Succ Id)) 'c'
-- Present "dddd"
-- PresentT "dddd"
--
-- >>> pz @(Repeat 4 "abc") ()
-- Present ["abc","abc","abc","abc"]
-- PresentT ["abc","abc","abc","abc"]
--
data Repeat (n :: Nat) p
instance P (RepeatT n p) a => P (Repeat n p) a where
  type PP (Repeat n p) a = PP (RepeatT n p) a
  eval _ opts a =
    eval (Proxy @(RepeatT n p)) opts a

-- | leverages 'Do' for repeating predicates (passthrough method)
-- same as @DoN n p == FoldN n p Id@ but more efficient
--
-- >>> pz @(DoN 4 (Succ Id)) 'c'
-- Present 'g'
-- PresentT 'g'
--
-- >>> pz @(DoN 4 (Id <> " | ")) "abc"
-- Present "abc |  |  |  | "
-- PresentT "abc |  |  |  | "
--
-- >>> pz @(DoN 4 (Id <> "|" <> Id)) "abc"
-- Present "abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc"
-- PresentT "abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc|abc"
--
data DoN (n :: Nat) p
type DoNT (n :: Nat) p = Do (RepeatT n p)
instance P (DoNT n p) a => P (DoN n p) a where
  type PP (DoN n p) a = PP (DoNT n p) a
  eval _ opts a =
    eval (Proxy @(DoNT n p)) opts a

-- | extract the value from a 'Maybe' otherwise use the default value
--
-- >>> pz @(JustDef (1 % 4) Id) (Just 20.4)
-- Present 102 % 5
-- PresentT (102 % 5)
--
-- >>> pz @(JustDef (1 % 4) Id) Nothing
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(JustDef (MEmptyT _) Id) (Just "xy")
-- Present "xy"
-- PresentT "xy"
--
-- >>> pz @(JustDef (MEmptyT _) Id) Nothing
-- Present ()
-- PresentT ()
--
-- >>> pz @(JustDef (MEmptyT (SG.Sum _)) Id) Nothing
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
data JustDef p q

instance ( PP p x ~ a
         , PP q x ~ Maybe a
         , P p x
         , P q x)
    => P (JustDef p q) x where
  type PP (JustDef p q) x = MaybeT (PP q x)
  eval _ opts x = do
    let msg0 = "JustDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Just b -> pure $ mkNode opts (PresentT b) [msg0 <> " Just"] [hh qq]
          Nothing -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [msg0 <> " Nothing"] [hh qq, hh pp]


type family MaybeT mb where
  MaybeT (Maybe a) = a
  MaybeT o = GL.TypeError (
      'GL.Text "MaybeT: expected 'Maybe a' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract the value from a 'Maybe' or fail
--
-- >>> pz @(JustFail "nope" Id) (Just 99)
-- Present 99
-- PresentT 99
--
-- >>> pz @(JustFail "nope" Id) Nothing
-- Error nope
-- FailT "nope"
--
-- >>> pz @(JustFail (PrintF "oops=%d" (Snd Id)) (Fst Id)) (Nothing, 123)
-- Error oops=123
-- FailT "oops=123"
--
-- >>> pz @(JustFail (PrintF "oops=%d" (Snd Id)) (Fst Id)) (Just 'x', 123)
-- Present 'x'
-- PresentT 'x'
--
data JustFail p q

instance ( PP p x ~ String
         , PP q x ~ Maybe a
         , P p x
         , P q x)
    => P (JustFail p q) x where
  type PP (JustFail p q) x = MaybeT (PP q x)
  eval _ opts x = do
    let msg0 = "JustFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Just b -> pure $ mkNode opts (PresentT b) [msg0 <> " Just"] [hh qq]
          Nothing -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " Nothing"] [hh qq, hh pp]

-- | extract the Left value from an 'Either' otherwise use the default value
--
-- if there is no Left value then \p\ is passed the Right value and the whole context
--
-- >>> pz @(LeftDef (1 % 4) Id) (Left 20.4)
-- Present 102 % 5
-- PresentT (102 % 5)
--
-- >>> pz @(LeftDef (1 % 4) Id) (Right "aa")
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(LeftDef (PrintT "found right=%s fst=%d" '(Fst Id,Fst (Snd Id))) (Snd Id)) (123,Right "xy")
-- Present "found right=xy fst=123"
-- PresentT "found right=xy fst=123"
--
-- >>> pz @(LeftDef (MEmptyT _) Id) (Right 222)
-- Present ()
-- PresentT ()
--
-- >>> pz @(LeftDef (MEmptyT (SG.Sum _)) Id) (Right 222)
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
data LeftDef p q

instance ( PP q x ~ Either a b
         , PP p (b,x) ~ a
         , P q x
         , P p (b,x)
    ) => P (LeftDef p q) x where
  type PP (LeftDef p q) x = LeftT (PP q x)
  eval _ opts x = do
    let msg0 = "LeftDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Left a -> pure $ mkNode opts (PresentT a) [msg0 <> " Left"] [hh qq]
          Right b -> do
            pp <- eval (Proxy @p) opts (b,x)
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) [msg0 <> " Right"] [hh qq, hh pp]

type family LeftT lr where
  LeftT (Either a b) = a
  LeftT o = GL.TypeError (
      'GL.Text "LeftT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family RightT lr where
  RightT (Either a b) = b
  RightT o = GL.TypeError (
      'GL.Text "RightT: expected 'Either a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | extract the Right value from an 'Either'
--
-- if there is no Right value then \p\ is passed the Left value and the whole context
--
-- >>> pz @(RightDef (1 % 4) Id) (Right 20.4)
-- Present 102 % 5
-- PresentT (102 % 5)
--
-- >>> pz @(RightDef (1 % 4) Id) (Left "aa")
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(RightDef (PrintT "found left=%s fst=%d" '(Fst Id,Fst (Snd Id))) (Snd Id)) (123,Left "xy")
-- Present "found left=xy fst=123"
-- PresentT "found left=xy fst=123"
--
-- >>> pz @(RightDef (MEmptyT _) Id) (Left 222)
-- Present ()
-- PresentT ()
--
-- >>> pz @(RightDef (MEmptyT (SG.Sum _)) Id) (Left 222)
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
data RightDef p q

instance ( PP q x ~ Either a b
         , PP p (a,x) ~ b
         , P q x
         , P p (a,x)
    ) => P (RightDef p q) x where
  type PP (RightDef p q) x = RightT (PP q x)
  eval _ opts x = do
    let msg0 = "RightDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Right b -> pure $ mkNode opts (PresentT b) [msg0 <> " Right"] [hh qq]
          Left a -> do
            pp <- eval (Proxy @p) opts (a,x)
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) [msg0 <> " Left"] [hh qq, hh pp]


-- | extract the Left value from an 'Either' otherwise fail with a message
--
-- if there is no Left value then \p\ is passed the Right value and the whole context
--
-- >>> pz @(LeftFail "oops" Id) (Left 20.4)
-- Present 20.4
-- PresentT 20.4
--
-- >>> pz @(LeftFail "oops" Id) (Right "aa")
-- Error oops
-- FailT "oops"
--
-- >>> pz @(LeftFail (PrintT "found right=%s fst=%d" '(Fst Id,Fst (Snd Id))) (Snd Id)) (123,Right "xy")
-- Error found right=xy fst=123
-- FailT "found right=xy fst=123"
--
-- >>> pz @(LeftFail (MEmptyT _) Id) (Right 222)
-- Error
-- FailT ""
--
data LeftFail p q

instance ( PP p (b,x) ~ String
         , PP q x ~ Either a b
         , P p (b,x)
         , P q x)
    => P (LeftFail p q) x where
  type PP (LeftFail p q) x = LeftT (PP q x)
  eval _ opts x = do
    let msg0 = "LeftFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Left a -> pure $ mkNode opts (PresentT a) [msg0 <> " Left"] [hh qq]
          Right b -> do
            pp <- eval (Proxy @p) opts (b,x)
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " Right"] [hh qq, hh pp]


-- | extract the Right value from an 'Either' otherwise fail with a message
--
-- if there is no Right value then \p\ is passed the Left value and the whole context
--
-- >>> pz @(RightFail "oops" Id) (Right 20.4)
-- Present 20.4
-- PresentT 20.4
--
-- >>> pz @(RightFail "oops" Id) (Left "aa")
-- Error oops
-- FailT "oops"
--
-- >>> pz @(RightFail (PrintT "found left=%s fst=%d" '(Fst Id,Fst (Snd Id))) (Snd Id)) (123,Left "xy")
-- Error found left=xy fst=123
-- FailT "found left=xy fst=123"
--
-- >>> pz @(RightFail (MEmptyT _) Id) (Left 222)
-- Error
-- FailT ""
--
data RightFail p q

instance ( PP p (a,x) ~ String
         , PP q x ~ Either a b
         , P p (a,x)
         , P q x)
    => P (RightFail p q) x where
  type PP (RightFail p q) x = RightT (PP q x)
  eval _ opts x = do
    let msg0 = "RightFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          Right b -> pure $ mkNode opts (PresentT b) [msg0 <> " Right"] [hh qq]
          Left a -> do
            pp <- eval (Proxy @p) opts (a,x)
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " Left"] [hh qq, hh pp]



-- | extract the This value from an 'These' otherwise use the default value
--
-- if there is no This value then \p\ is passed the whole context only
--
-- >>> pz @(ThisDef (1 % 4) Id) (This 20.4)
-- Present 102 % 5
-- PresentT (102 % 5)
--
-- >>> pz @(ThisDef (1 % 4) Id) (That "aa")
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(ThisDef (1 % 4) Id) (These 2.3 "aa")
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(ThisDef (PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id)) (Snd Id)) (123,That "xy")
-- Present "found That \"xy\" fst=123"
-- PresentT "found That \"xy\" fst=123"
--
-- >>> pz @(ThisDef (MEmptyT _) Id) (That 222)
-- Present ()
-- PresentT ()
--
-- >>> pz @(ThisDef (MEmptyT (SG.Sum _)) Id) (These 222 'x')
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
data ThisDef p q

instance ( PP q x ~ These a b
         , PP p x ~ a
         , P q x
         , P p x
    ) => P (ThisDef p q) x where
  type PP (ThisDef p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          This a -> pure $ mkNode opts (PresentT a) [msg0 <> " This"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]

type family ThisT lr where
  ThisT (These a b) = a
  ThisT o = GL.TypeError (
      'GL.Text "ThisT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family ThatT lr where
  ThatT (These a b) = b
  ThatT o = GL.TypeError (
      'GL.Text "ThatT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

type family TheseT lr where
  TheseT (These a b) = (a,b)
  TheseT o = GL.TypeError (
      'GL.Text "TheseT: expected 'These a b' "
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)


-- | extract the That value from an 'These' otherwise use the default value
--
-- if there is no That value then \p\ is passed the whole context only
--
-- >>> pz @(ThatDef (1 % 4) Id) (That 20.4)
-- Present 102 % 5
-- PresentT (102 % 5)
--
-- >>> pz @(ThatDef (1 % 4) Id) (This "aa")
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(ThatDef (1 % 4) Id) (These "aa" 2.3)
-- Present 1 % 4
-- PresentT (1 % 4)
--
-- >>> pz @(ThatDef (PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id)) (Snd Id)) (123,This "xy")
-- Present "found This \"xy\" fst=123"
-- PresentT "found This \"xy\" fst=123"
--
-- >>> pz @(ThatDef (MEmptyT _) Id) (This 222)
-- Present ()
-- PresentT ()
--
-- >>> pz @(ThatDef (MEmptyT (SG.Sum _)) Id) (These 'x' 1120)
-- Present Sum {getSum = 0}
-- PresentT (Sum {getSum = 0})
--
data ThatDef p q

instance ( PP q x ~ These a b
         , PP p x ~ b
         , P q x
         , P p x
    ) => P (ThatDef p q) x where
  type PP (ThatDef p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          That a -> pure $ mkNode opts (PresentT a) [msg0 <> " That"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]

-- | extract the These value from an 'These' otherwise use the default value
--
-- if there is no These value then \p\ is passed the whole context only
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (These 20.4 "x")
-- Present (102 % 5,"x")
-- PresentT (102 % 5,"x")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (This 20.4)
-- Present (1 % 4,"zz")
-- PresentT (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(1 % 4,"zz") Id) (That "x")
-- Present (1 % 4,"zz")
-- PresentT (1 % 4,"zz")
--
-- >>> pz @(TheseDef '(PrintT "found %s fst=%d" '(ShowP (Snd Id), Fst Id),999) (Snd Id)) (123,This "xy")
-- Present ("found This \"xy\" fst=123",999)
-- PresentT ("found This \"xy\" fst=123",999)
--
-- >>> pz @(TheseDef (MEmptyT (SG.Sum _, String)) Id) (This 222)
-- Present (Sum {getSum = 0},"")
-- PresentT (Sum {getSum = 0},"")
--
-- >>> pz @(TheseDef (MEmptyT _) Id) (These (222 :: SG.Sum Int) "aa")
-- Present (Sum {getSum = 222},"aa")
-- PresentT (Sum {getSum = 222},"aa")
--
data TheseDef p q

instance ( PP q x ~ These a b
         , PP p x ~ (a,b)
         , P q x
         , P p x
    ) => P (TheseDef p q) x where
  type PP (TheseDef p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseDef"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          These a b -> pure $ mkNode opts (PresentT (a,b)) [msg0 <> " These"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (PresentT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]


-- | extract the This value from a 'These' otherwise fail with a message
--
-- if there is no This value then \p\ is passed the whole context only
--
-- >>> pz @(ThisFail "oops" Id) (This 20.4)
-- Present 20.4
-- PresentT 20.4
--
-- >>> pz @(ThisFail "oops" Id) (That "aa")
-- Error oops
-- FailT "oops"
--
-- >>> pz @(ThisFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,That "xy")
-- Error found That "xy" fst=123
-- FailT "found That \"xy\" fst=123"
--
-- >>> pz @(ThisFail (MEmptyT _) Id) (That 222)
-- Error
-- FailT ""
--
data ThisFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (ThisFail p q) x where
  type PP (ThisFail p q) x = ThisT (PP q x)
  eval _ opts x = do
    let msg0 = "ThisFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          This a -> pure $ mkNode opts (PresentT a) [msg0 <> " This"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]


-- | extract the That value from a 'These' otherwise fail with a message
--
-- if there is no That value then \p\ is passed the whole context only
--
-- >>> pz @(ThatFail "oops" Id) (That 20.4)
-- Present 20.4
-- PresentT 20.4
--
-- >>> pz @(ThatFail "oops" Id) (This "aa")
-- Error oops
-- FailT "oops"
--
-- >>> pz @(ThatFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,This "xy")
-- Error found This "xy" fst=123
-- FailT "found This \"xy\" fst=123"
--
-- >>> pz @(ThatFail (MEmptyT _) Id) (This 222)
-- Error
-- FailT ""
--
data ThatFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (ThatFail p q) x where
  type PP (ThatFail p q) x = ThatT (PP q x)
  eval _ opts x = do
    let msg0 = "ThatFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          That a -> pure $ mkNode opts (PresentT a) [msg0 <> " That"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]




-- | extract the These value from a 'These' otherwise fail with a message
--
-- if there is no These value then \p\ is passed the whole context only
--
-- >>> pz @(TheseFail "oops" Id) (These "abc" 20.4)
-- Present ("abc",20.4)
-- PresentT ("abc",20.4)
--
-- >>> pz @(TheseFail "oops" Id) (That "aa")
-- Error oops
-- FailT "oops"
--
-- >>> pz @(TheseFail (PrintT "found %s fst=%d" '(ShowP (Snd Id),Fst Id)) (Snd Id)) (123,That "xy")
-- Error found That "xy" fst=123
-- FailT "found That \"xy\" fst=123"
--
-- >>> pz @(TheseFail (MEmptyT _) Id) (That 222)
-- Error
-- FailT ""
--
data TheseFail p q

instance ( PP p x ~ String
         , PP q x ~ These a b
         , P p x
         , P q x)
    => P (TheseFail p q) x where
  type PP (TheseFail p q) x = TheseT (PP q x)
  eval _ opts x = do
    let msg0 = "TheseFail"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        case q of
          These a b -> pure $ mkNode opts (PresentT (a,b)) [msg0 <> " These"] [hh qq]
          _ -> do
            pp <- eval (Proxy @p) opts x
            pure $ case getValueLR opts msg0 pp [hh qq] of
              Left e -> e
              Right p -> mkNode opts (FailT p) [msg0 <> " " <> showThese q] [hh qq, hh pp]

-- | takes the head of a list like container
--
-- >>> pz @(Head Id) "abcd"
-- Present 'a'
-- PresentT 'a'
--
-- >>> pz @(Head Id) []
-- Error Head(empty)
-- FailT "Head(empty)"
--
data Head p

instance (Show (ConsT s)
        , Show s
        , Cons s s (ConsT s) (ConsT s)
        , PP p x ~ s
        , P p x
        ) => P (Head p) x where
  type PP (Head p) x = ConsT (PP p x)
  eval _ opts x = do
    let msg0 = "Head"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p ^? _Cons of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) [msg0 <> " no data"] [hh pp]
          Just (a,_) -> mkNode opts (PresentT a) [show01 opts msg0 a p] [hh pp]

-- | takes the tail of a list like container
--
-- >>> pz @(Tail Id) "abcd"
-- Present "bcd"
-- PresentT "bcd"
--
-- >>> pz @(Tail Id) []
-- Error Tail(empty)
-- FailT "Tail(empty)"
--
data Tail p

instance (Show s
        , Cons s s (ConsT s) (ConsT s)
        , PP p x ~ s
        , P p x
        ) => P (Tail p) x where
  type PP (Tail p) x = PP p x
  eval _ opts x = do
    let msg0 = "Tail"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p ^? _Cons of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) [msg0 <> " no data"] [hh pp]
          Just (_,as) -> mkNode opts (PresentT as) [show01 opts msg0 as p] [hh pp]


-- | takes the last of a list like container
--
-- >>> pz @(Last Id) "abcd"
-- Present 'd'
-- PresentT 'd'
--
-- >>> pz @(Last Id) []
-- Error Last(empty)
-- FailT "Last(empty)"
--

data Last p

instance (Show (ConsT s)
        , Show s
        , Snoc s s (ConsT s) (ConsT s)
        , PP p x ~ s
        , P p x
        ) => P (Last p) x where
  type PP (Last p) x = ConsT (PP p x)
  eval _ opts x = do
    let msg0 = "Last"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p ^? _Snoc of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) [msg0 <> " no data"] [hh pp]
          Just (_,a) -> mkNode opts (PresentT a) [show01 opts msg0 a p] [hh pp]

-- | takes the init of a list like container
--
-- >>> pz @(Init Id) "abcd"
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(Init Id) (T.pack "abcd")
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(Init Id) []
-- Error Init(empty)
-- FailT "Init(empty)"
--

data Init p

instance (Show s
        , Snoc s s (ConsT s) (ConsT s)
        , PP p x ~ s
        , P p x
        ) => P (Init p) x where
  type PP (Init p) x = PP p x
  eval _ opts x = do
    let msg0 = "Init"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p ^? _Snoc of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) [msg0 <> " no data"] [hh pp]
          Just (as,_) -> mkNode opts (PresentT as) [show01 opts msg0 as p] [hh pp]


-- | tries to extract @a@ from @Maybe a@ otherwise it fails
--
-- >>> pz @(Just Id) (Just "abc")
-- Present "abc"
-- PresentT "abc"
--
-- >>> pz @(Just Id) Nothing
-- Error Just(empty)
-- FailT "Just(empty)"
--
data Just p

instance (Show a
        , PP p x ~ Maybe a
        , P p x
        ) => P (Just p) x where
  type PP (Just p) x = MaybeT (PP p x)
  eval _ opts x = do
    let msg0 = "Just"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        case p of
          Nothing -> mkNode opts (FailT (msg0 <> "(empty)")) [msg0 <> " found Nothing"] [hh pp]
          Just d -> mkNode opts (PresentT d) [show01 opts msg0 d p] [hh pp]


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

-- | like 'GHC.Base.$' for expressions
--
-- >>> pl @(Fst $ Snd $ Id) ((1,2),(3,4))
-- Present 3 (Fst 3 | (3,4))
-- PresentT 3
--
-- >>> pl @((<=) 4 $ Fst $ Snd $ Id) ((1,2),(3,4))
-- False (4 <= 3)
-- FalseT
--
data (p :: Type -> Type) $ (q :: Type)
infixr 0 $

instance P (p q) a => P (p $ q) a where
  type PP (p $ q) a = PP (p q) a
  eval _ opts a = do
    eval (Proxy @(p q)) opts a


-- | similar to 'Control.Lens.&'
--
-- >>> pl @(Id & Fst & Singleton & Length) (13,"xyzw")
-- Present 1 (Length 1 | [13])
-- PresentT 1
--
data (q :: Type) & (p :: Type -> Type)
infixl 1 &

instance P (p q) a => P (q & p) a where
  type PP (q & p) a = PP (p q) a
  eval _ opts a = do
    eval (Proxy @(p q)) opts a

-- | creates a constant expression ignoring the second arguenent
--
-- >>> pl @(RDot '[Fst,Snd,Thd,K "xxx"] Id) ((1,(2,9,10)),(3,4))
-- Present "xxx" (K'xxx)
-- PresentT "xxx"
--
-- >>> pl @(RDot '[Fst,Snd,Thd,K '("abc",Id)] Id) ((1,(2,9,10)),(3,4))
-- Present ("abc",((1,(2,9,10)),(3,4))) (K'(,))
-- PresentT ("abc",((1,(2,9,10)),(3,4)))
--
data K (p :: k) (q :: Type)
instance P p a => P (K p q) a where
  type PP (K p q) a = PP p a
  eval _ = eval (Proxy @(Msg "K" p))

-- | applies \'p\' to the first and second slot of an n-tuple
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17],1,2,3),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
--
-- >>> pl @(Both (Pred Id) $ Fst Id) ((12,'z',[10..17]),True)
-- Present (11,'y') (Both)
-- PresentT (11,'y')
--
-- >>> pl @(Both (Succ Id) Id) (4,'a')
-- Present (5,'b') (Both)
-- PresentT (5,'b')
--
-- >>> pl @(Both Len (Fst Id)) (("abc",[10..17]),True)
-- Present (3,8) (Both)
-- PresentT (3,8)
data Both p q
instance ( ExtractL1C (PP q x)
         , ExtractL2C (PP q x)
         , P p (ExtractL1T (PP q x))
         , P p (ExtractL2T (PP q x))
         , P q x
   ) => P (Both p q) x where
  type PP (Both p q) x = (PP p (ExtractL1T (PP q x)), PP p (ExtractL2T (PP q x)))
  eval _ opts x = do
    let msg0 = "Both"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right q -> do
        let (a,a') = (extractL1C q, extractL2C q)
        pp <- eval (Proxy @p) opts a
        case getValueLR opts msg0 pp [hh qq] of
          Left e -> pure e
          Right b -> do
            pp' <- eval (Proxy @p) opts a'
            pure $ case getValueLR opts msg0 pp' [hh qq, hh pp] of
              Left e -> e
              Right b' ->
                mkNode opts (PresentT (b,b')) [msg0] [hh qq, hh pp, hh pp']

-- | gets the singleton value from a foldable
--
-- >>> pl @(OneP Id) [10..15]
-- Error OneP 6 elements
-- FailT "OneP 6 elements"
--
-- >>> pl @(OneP Id) [10]
-- Present 10 (OneP)
-- PresentT 10
--
-- >>> pl @(OneP Id) []
-- Error OneP empty
-- FailT "OneP empty"
--
-- >>> pl @(OneP Id) (Just 10)
-- Present 10 (OneP)
-- PresentT 10
--
-- >>> pl @(OneP Id) Nothing
-- Error OneP empty
-- FailT "OneP empty"
--
data OneP p
instance (Foldable t
        , PP p x ~ t a
        , P p x
        ) => P (OneP p) x where
  type PP (OneP p) x = ExtractAFromTA (PP p x)
  eval _ opts x = do
    let msg0 = "OneP"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p -> case toList p of
                   [] -> mkNode opts (FailT (msg0 <> " empty")) [msg0 <> " expected one element"] [hh pp]
                   [a] -> mkNode opts (PresentT a) [msg0] [hh pp]
                   as -> let n = length as
                         in mkNode opts (FailT (msg0 <> " " <> show n <> " elements")) [msg0 <> " expected one element"] [hh pp]

-- | parse json data
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\"]"
-- Present (10,"abc") (ParseJson (Int,[Char]) (10,"abc"))
-- PresentT (10,"abc")
--
-- >>> pl @(ParseJson (Int,String) Id) "[10,\"abc\",99]"
-- Error ParseJson (Int,[Char])([10,"abc",...) Error in $
-- FailT "ParseJson (Int,[Char])([10,\"abc\",...) Error in $"
--
data ParseJson' t p

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , A.FromJSON (PP t x)
        ) => P (ParseJson' t p) x where
  type PP (ParseJson' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ParseJson " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let hhs = [hh pp]
            msg1 = msg0 <> "(" ++ litL 10 s ++ ")"
        in case A.eitherDecodeStrict' (BS8.pack s) of
           Right b -> mkNode opts (PresentT b) [msg0 <> " " ++ showL 30 b] hhs
           Left e -> mkNode opts (FailT (msg1 <> " " <> takeWhile (/=':') e)) [msg0 <> " failed " <> e <> " | " <> litL 100 s] hhs

data ParseJson (t :: Type) p
type ParseJsonT (t :: Type) p = ParseJson' (Hole t) p

instance P (ParseJsonT t p) x => P (ParseJson t p) x where
  type PP (ParseJson t p) x = PP (ParseJsonT t p) x
  eval _ = eval (Proxy @(ParseJsonT t p))

-- | encode json
--
-- >>> pl @(EncodeJson Id) (10,"def")
-- Present "[10,\"def\"]" (EncodeJson [10,"def"])
-- PresentT "[10,\"def\"]"
--
data EncodeJson p

instance (A.ToJSON (PP p x), P p x) => P (EncodeJson p) x where
  type PP (EncodeJson p) x = String
  eval _ opts x = do
    let msg0 = "EncodeJson"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = BL8.unpack (A.encode p)
        in mkNode opts (PresentT d) [msg0 <> showLit0 opts " " d] [hh pp]

-- | parse a json file
data ParseJsonFile' t p

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , A.FromJSON (PP t x)
        ) => P (ParseJsonFile' t p) x where
  type PP (ParseJsonFile' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ParseJsonFile " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    case getValueLR opts msg0 pp [] of
      Left e -> pure e
      Right p -> do
        let hhs = [hh pp]
            msg1 = msg0 <> "(" <> p <> ")"
        mb <- runIO $ do
                b <- doesFileExist p
                if b then Just <$> BS8.readFile p
                else pure Nothing
        pure $ case mb of
          Nothing -> mkNode opts (FailT (msg1 <> " must run in IO")) [msg1 <> " must run in IO"] hhs
          Just Nothing -> mkNode opts (FailT (msg1 <> " file doesn't exist")) [msg1 <> " does not exist"] hhs
          Just (Just bs) ->
            case A.eitherDecodeStrict' bs of
               Right b -> mkNode opts (PresentT b) [msg1 <> " " ++ show b] hhs
               Left e -> mkNode opts (FailT (msg1 <> " " <> takeWhile (/=':') e)) [msg1 <> " failed " <> litL 100 e] hhs

data ParseJsonFile (t :: Type) p
type ParseJsonFileT (t :: Type) p = ParseJsonFile' (Hole t) p

instance P (ParseJsonFileT t p) x => P (ParseJsonFile t p) x where
  type PP (ParseJsonFile t p) x = PP (ParseJsonFileT t p) x
  eval _ = eval (Proxy @(ParseJsonFileT t p))


