{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedLists #-}
{- |
Module      : Predicate
Description : Dsl for evaluating and displaying type level expressions
Copyright   : (c) Grant Weyburne, 2019
License     : BSD-3
Maintainer  : gbwey9@gmail.com

class P is the main class. Most of this code contains instances of this class
that evaluation of expressions at the type level.
-}
module Predicate where
import UtilP
import Safe
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import qualified GHC.TypeNats as GN
import Control.Lens hiding (strict,iall)
import Data.List
import Data.Text.Lens
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
import Numeric
import Data.Char
import Data.Function
import Data.These
import Data.These.Lens ()
import qualified Data.Align as TA
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
import qualified GHC.Exts as Ge
import Data.Bool
import Data.Either
import qualified Data.Type.Equality as DE
import Data.Time.Calendar.WeekDate

-- | This is the core class. Each instance of this class can be combined into a dsl using 'Main.>>'
class P p a where
  type PP (p :: k) a :: Type -- PP is the output type
  eval :: MonadEval m => Proxy p -> POpts -> a -> m (TT (PP p a)) -- ^ returns a tree of results

-- | A specialised form of 'eval' that works only on predicates
evalBool :: (MonadEval m, P p a, PP p a ~ Bool) => Proxy p -> POpts -> a -> m (TT (PP p a))
evalBool p opts a = fixBoolT <$> eval p opts a

-- | a type level predicate for a monotonic increasing list
type Asc = Map (Fst <= Snd) Pairs >> Ands
-- | a type level predicate for a strictly increasing list
type Asc' = Map (Fst < Snd) Pairs >> Ands
-- | a type level predicate for a monotonic decreasing list
type Desc = Map (Fst >= Snd) Pairs >> Ands
-- | a type level predicate for a strictly decreasing list
type Desc' = Map (Fst > Snd) Pairs >> Ands

-- | A predicate that determines if the value is between \'p\' and \'q\'
--   The values can be rational numbers using 'Rat' or plain Natural numbers
type Between p q = Ge p && Le q
-- | This is the same as 'Between' but where \'r\' is 'Id'
type Between' p q r = r >= p && r <= q

-- | a type level predicate for all positive elements in a list
type AllPositive = Map Positive >> Ands
-- | a type level predicate for all negative elements in a list
type AllNegative = Map Negative >> Ands
type Positive = Ge 0
type Negative = Le 0

type AllPositive' = FoldMap SG.All (Map Positive Id)
type AllNegative' = FoldMap SG.All (Map Negative Id)

type All x = Map x Id >> Ands
type Any x = Map x Id >> Ors

-- | 'unzip' equivalent
type Unzip = (Map Fst Id, Map Snd Id)

-- | represents a predicate using a 'Symbol' as a regular expression
--   evaluates 'Re' and returns True if there is a match
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Re "^\\d{2}:\\d{2}:\\d{2}$" Id) "13:05:25"
--   True
--   TrueT
--
data Re' (rs :: [ROpt]) p q
type Re p q = Re' '[] p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
            Left tta -> tta
            Right regex ->
               let b = q RH.=~ regex
               in mkNodeB opts b [msg1 <> showLit opts " | " q] hhs

-- only way with rescan is to be explicit: no repeats! and useanchors but not (?m)
-- or just use Re' but then we only get a bool ie doesnt capture groups
-- rescan returns Right [] as an failure!
-- [] is failure!


-- | runs a regex matcher returning the original values and optionally any groups
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
--   Present [("13:05:25",["13","05","25"])]
--   PresentT [("13:05:25",["13","05","25"])]
--
data Rescan' (rs :: [ROpt]) p q
type Rescan p q = Rescan' '[] p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.scan regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> showA opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> showA opts " | " q] [hh pp, hh qq]
              (b, _) -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit opts " | " q] [hh pp, hh qq]


-- | similar to 'Rescan' but gives the column start and ending positions instead of values
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(RescanRanges "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
--   Present [((0,8),[(0,2),(3,5),(6,8)])]
--   PresentT [((0,8),[(0,2),(3,5),(6,8)])]
--
data RescanRanges' (rs :: [ROpt]) p q
type RescanRanges p q = RescanRanges' '[] p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.scanRanges regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> showA opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> showA opts " | " q] hhs
              (b, _) -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit opts " | " q] hhs

-- | splits a string on a regex delimiter
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Resplit "\\." Id) "141.201.1.22"
--   Present ["141","201","1","22"]
--   PresentT ["141","201","1","22"]
--
data Resplit' (rs :: [ROpt]) p q
type Resplit p q = Resplit' '[] p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Resplit' rs p q) x where
  type PP (Resplit' rs p q) x  = [String]
  eval _ opts x = do
    let msg0 = "Resplit" <> (if null rs then "' " <> show rs else "")
        rs = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt _MX $ RH.split regex q of
              (b, _:_) -> mkNode opts (FailT "Regex looping") [msg1 <> " Looping? " <> show (take 10 b) <> "..." <> showA opts " | " q] hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") [msg1 <> " no match" <> showA opts " | " q] hhs
              (b, _) -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit opts " | " q] hhs

_MX :: Int
_MX = 100

-- | replaces regex \'s\' with a string \'s1\' inside the value
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ReplaceAllString "\\." ":" Id) "141.201.1.22"
--   Present "141:201:1:22"
--   PresentT "141:201:1:22"
--
data ReplaceImpl (alle :: Bool) (rs :: [ROpt]) p q r
type ReplaceAll' (rs :: [ROpt]) p q r = ReplaceImpl 'True rs p q r
type ReplaceAll p q r = ReplaceAll' '[] p q r
type ReplaceOne' (rs :: [ROpt]) p q r = ReplaceImpl 'False rs p q r
type ReplaceOne p q r = ReplaceOne' '[] p q r

type ReplaceAllString' (rs :: [ROpt]) p q r = ReplaceAll' rs p (MakeRR q) r
type ReplaceAllString p q r = ReplaceAllString' '[] p q r

type ReplaceOneString' (rs :: [ROpt]) p q r = ReplaceOne' rs p (MakeRR q) r
type ReplaceOneString p q r = ReplaceOneString' '[] p q r

-- | Simple replacement string: see 'ReplaceAllString' and 'ReplaceOneString'
--
data MakeRR p

instance (PP p x ~ String
        , P p x) => P (MakeRR p) x where
  type PP (MakeRR p) x = RR
  eval _ opts x = do
    let msg0 = "MakeRR"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right as ->
        let b = RR as
        in mkNode opts (PresentT b) [msg0 <> showA opts " | " as] [hh pp]

-- | A replacement function (String -> [String] -> String) which returns the whole match and the groups
--   Used by 'RH.sub' and 'RH.sub'
--   Requires "Text.Show.Functions"
--
data MakeRR1 p

instance (PP p x ~ (String -> [String] -> String)
        , P p x) => P (MakeRR1 p) x where
  type PP (MakeRR1 p) x = RR
  eval _ opts x = do
    let msg0 = "MakeRR1 (String -> [String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RR1 f)) [msg0] [hh pp]

-- | A replacement function (String -> String) that yields the whole match
--   Used by 'RH.sub' and 'RH.sub'
--   Requires "Text.Show.Functions"
--
--   >>> :m + Text.Show.Functions
--   >>> pl @(ReplaceAll "\\." (MakeRR2 Fst) Snd) (\x -> x <> ":" <> x, "141.201.1.22")
--   Present "141.:.201.:.1.:.22"
--   PresentT "141.:.201.:.1.:.22"
--
data MakeRR2 p

instance (PP p x ~ (String -> String)
        , P p x) => P (MakeRR2 p) x where
  type PP (MakeRR2 p) x = RR
  eval _ opts x = do
    let msg0 = "MakeRR2 (String -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RR2 f)) [msg0] [hh pp]

-- | A replacement function ([String] -> String) which yields the groups
--   Used by 'RH.sub' and 'RH.sub'
--   Requires "Text.Show.Functions"
--
--   >>> :m + Text.Show.Functions
--   >>> pl @(ReplaceAll "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" (MakeRR3 Fst) Snd) (\ys -> intercalate  " | " $ map (show . succ . read @Int) ys, "141.201.1.22")
--   Present "142 | 202 | 2 | 23"
--   PresentT "142 | 202 | 2 | 23"
--
data MakeRR3 p

instance (PP p x ~ ([String] -> String)
        , P p x) => P (MakeRR3 p) x where
  type PP (MakeRR3 p) x = RR
  eval _ opts x = do
    let msg0 = "MakeRR3 ([String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RR3 f)) [msg0] [hh pp]

instance (GetBool b
        , GetROpts rs
        , PP p x ~ String
        , PP q x ~ RR
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
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
                           RR s -> (if alle then RH.gsub else RH.sub) regex s r
                           RR1 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RR2 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RR3 s -> (if alle then RH.gsub else RH.sub) regex s r
               in mkNode opts (PresentT ret) [msg1 <> showLit opts " " r <> showLit opts " | " ret] (hhs <> [hh rr])

-- | a predicate for determining if a string 'Data.Text.IsText' belongs to the given character set
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> import qualified Data.Text as T
--   >>> pl @IsLower "abc"
--   True
--   TrueT
--
--   >>> pl @IsLower "abcX"
--   False
--   FalseT
--
--   >>> pl @IsLower (T.pack "abcX")
--   False
--   FalseT
--
--   >>> pl @IsHexDigit "01efA"
--   True
--   TrueT
--
--   >>> pl @IsHexDigit "01egfA"
--   False
--   FalseT
--
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

type IsLower = IsCharSet 'CLower
type IsUpper = IsCharSet 'CUpper
type IsNumber = IsCharSet 'CNumber
type IsSpace = IsCharSet 'CSpace
type IsPunctuation = IsCharSet 'CPunctuation
type IsControl = IsCharSet 'CControl
type IsHexDigit = IsCharSet 'CHexDigit
type IsOctDigit = IsCharSet 'COctDigit
type IsSeparator = IsCharSet 'CSeparator
type IsLatin1 = IsCharSet 'CLatin1

instance (GetCharSet cs
        , Show a
        , IsText a
        ) => P (IsCharSet cs) a where
  type PP (IsCharSet cs) a = Bool
  eval _ opts as =
    let b = allOf text f as
        msg0 = "IsCharSet " ++ show cs
        (cs,f) = getCharSet @cs
    in pure $ mkNodeB opts b [msg0 <> showA opts " | " as] []


-- | converts a string 'Data.Text.Lens.IsText' value to lower case
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @ToLower "HeLlO wOrld!"
--   Present "hello world!"
--   PresentT "hello world!"
--
data ToLower

instance (Show a, IsText a) => P ToLower a where
  type PP ToLower a = a
  eval _ opts as =
    let xs = as & text %~ toLower
    in pure $ mkNode opts (PresentT xs) ["ToLower" <> show0 opts " " xs <> showA opts " | " as] []

-- | converts a string 'Data.Text.Lens.IsText' value to upper case
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @ToUpper "HeLlO wOrld!"
--   Present "HELLO WORLD!"
--   PresentT "HELLO WORLD!"
--
data ToUpper

instance (Show a, IsText a) => P ToUpper a where
  type PP ToUpper a = a
  eval _ opts as =
    let xs = as & text %~ toUpper
    in pure $ mkNode opts (PresentT xs) ["ToUpper" <> show0 opts " " xs <> showA opts " | " as] []


-- | similar to 'Data.List.inits'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Inits [4,8,3,9]
--   Present [[],[4],[4,8],[4,8,3],[4,8,3,9]]
--   PresentT [[],[4],[4,8],[4,8,3],[4,8,3,9]]
--
--   >>> pl @Inits []
--   Present [[]]
--   PresentT [[]]
--
data Inits

instance Show a => P Inits [a] where
  type PP Inits [a] = [[a]]
  eval _ opts as =
    let xs = inits as
    in pure $ mkNode opts (PresentT xs) ["Inits" <> show0 opts " " xs <> showA opts " | " as] []

-- | similar to 'Data.List.tails'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Tails [4,8,3,9]
--   Present [[4,8,3,9],[8,3,9],[3,9],[9],[]]
--   PresentT [[4,8,3,9],[8,3,9],[3,9],[9],[]]
--
--   >>> pl @Tails []
--   Present [[]]
--   PresentT [[]]
--
data Tails

instance Show a => P Tails [a] where
  type PP Tails [a] = [[a]]
  eval _ opts as =
    let xs = tails as
    in pure $ mkNode opts (PresentT xs) ["Tails" <> show0 opts " " xs <> showA opts " | " as] []

-- | split a list into single values
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Ones [4,8,3,9]
--   Present [[4],[8],[3],[9]]
--   PresentT [[4],[8],[3],[9]]
--
--   >>> pl @Ones []
--   Present []
--   PresentT []
--
data Ones

instance (as ~ [a], Show a) => P Ones as where
  type PP Ones as = [as]
  eval _ opts as =
    let xs = map (:[]) as
    in pure $ mkNode opts (PresentT xs) ["Ones" <> show0 opts " " xs <> showA opts " | " as] []

-- | similar to 'show'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @ShowP [4,8,3,9]
--   Present "[4,8,3,9]"
--   PresentT "[4,8,3,9]"
--
--   >>> pl @ShowP 'x'
--   Present "'x'"
--   PresentT "'x'"
--
data ShowP

instance Show as => P ShowP as where
  type PP ShowP as = String
  eval _ opts as =
    let x = show as
    in pure $ mkNode opts (PresentT x) ["ShowP" <> showLit0 opts " " x <> showA opts " | " as] []

-- | type level expression representing a formatted time
--   similar to 'Data.Time.formatTime' using a type level 'Symbol' to get the formatting string
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FormatTimeP "%F %T" Id) (read "2019-05-24 05:19:59" :: LocalTime)
--   Present "2019-05-24 05:19:59"
--   PresentT "2019-05-24 05:19:59"
--
--   >>> pl @(FormatTimeP Fst Snd) ("the date is %d/%m/%Y", read "2019-05-24" :: Day)
--   Present "the date is 24/05/2019"
--   PresentT "the date is 24/05/2019"
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            b = formatTime defaultTimeLocale p q
        in mkNode opts (PresentT b) [msg1 <> showLit0 opts " " b <> showA opts " | " q] [hh pp, hh qq]

-- | similar to 'Data.Time.parseTimeM' where \'t\' is the 'Data.Time.ParseTime' type, \'p\' is the datetime format and \'q\' points to the content to parse
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ParseTimeP LocalTime "%F %T" Id) "2019-05-24 05:19:59"
--   Present 2019-05-24 05:19:59
--   PresentT 2019-05-24 05:19:59
--
--   >>> pl @(ParseTimeP LocalTime "%F %T" "2019-05-24 05:19:59") (Right "we ignore this using Symbol and not Id")
--   Present 2019-05-24 05:19:59
--   PresentT 2019-05-24 05:19:59
--
-- keeping \'q\' as we might want to extract from a tuple
data ParseTimeP' t p q
type ParseTimeP (t :: Type) p q = ParseTimeP' (Hole t) p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case parseTimeM @Maybe @(PP t a) True defaultTimeLocale p q of
             Just b -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit0 opts " | fmt=" p <> showA opts " | " q] hhs
             Nothing -> mkNode opts (FailT (msg1 <> " failed to parse")) [msg1 <> " failed"] hhs

-- | A convenience method to match against many different datetime formats to find a match
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ParseTimes LocalTime '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"] "03/11/19 01:22:33") ()
--   Present 2019-03-11 01:22:33
--   PresentT 2019-03-11 01:22:33
--
--   >>> pl @(ParseTimes LocalTime Fst Snd) (["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"], "03/11/19 01:22:33")
--   Present 2019-03-11 01:22:33
--   PresentT 2019-03-11 01:22:33
--
data ParseTimes' t p q
type ParseTimes (t :: Type) p q = ParseTimes' (Hole t) p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0
            hhs = [hh pp, hh qq]
            zs = map (\d -> (d,) <$> parseTimeM @Maybe @(PP t a) True defaultTimeLocale d q) p
        in case catMaybes zs of
             [] -> mkNode opts (FailT ("no match on [" ++ q ++ "]")) [msg1 <> " no match"] hhs
             (d,b):_ -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit0 opts " | fmt=" d <> showA opts " | " q] hhs

-- | create a 'Day' from three int values passed in as year month and day
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :set -XTypeOperators
--   >>> pl @(MkDay Fst (Snd >> Fst) (Snd >> Snd)) (2019,(12,30))
--   Present Just (2019-12-30,(1,1))
--   PresentT (Just (2019-12-30,(1,1)))
--
--   >>> pl @(MkDay Fst (Snd >> Fst) (Snd >> Snd)) (2019,(99,99999))
--   Present Nothing
--   PresentT Nothing
--
--   >>> pl @(MkDay Fst (Snd >> Fst) (Snd >> Snd)) (1999,(3,13))
--   Present Just (1999-03-13,(10,6))
--   PresentT (Just (1999-03-13,(10,6)))
--
data MkDay p q r
instance (P p x
        , P q x
        , P r x
        , PP p x ~ Int
        , PP q x ~ Int
        , PP r x ~ Int
        ) => P (MkDay p q r) x where
  type PP (MkDay p q r) x = Maybe (Day, (Int, Int))
  eval _ opts x = do
    let msg0 = "MkDay"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
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
                      in (day, (week, dow))
            in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | (y,m,d)=" (p,q,r)] (hhs <> [hh rr])

-- | uncreate a 'Day' returning year month and day
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @UnMkDay (read "2019-12-30")
--   Present (2019,(12,30))
--   PresentT (2019,(12,30))
--
data UnMkDay
instance a ~ Day => P UnMkDay a where
  type PP UnMkDay a = (Int, (Int, Int))
  eval _ opts day =
    let msg0 = "UnMkDay"
        (y,m,d) = toGregorian day
        b = (fromIntegral y,(m,d))
    in pure $ mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " day] []

-- | uses the 'Read' of the given type \'t\' and \'p\' which points to the content to read
--
--   >>> :set -XTypeApplications
--   >>> :set -XTypeOperators
--   >>> :set -XDataKinds
--   >>> pl @(ReadP Rational) "4 % 5"
--   Present 4 % 5
--   PresentT (4 % 5)
--
--   >>> pl @(ReadP' Day Id >> Between (ReadP' Day "2017-04-11") (ReadP' Day "2018-12-30")) "2018-10-12"
--   True
--   TrueT
--
--   >>> pl @(ReadP' Day Id >> Between (ReadP' Day "2017-04-11") (ReadP' Day "2018-12-30")) "2016-10-12"
--   False
--   FalseT
--
data ReadP'' t p
type ReadP (t :: Type) = ReadP'' (Hole t) Id
type ReadP' (t :: Type) p = ReadP'' (Hole t) p

instance (P p x
        , PP p x ~ String
        , Typeable (PP t x)
        , Show (PP t x)
        , Read (PP t x)
        ) => P (ReadP'' t p) x where
  type PP (ReadP'' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ReadP " <> t
        t = showT @(PP t x)
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right s ->
        let msg1 = msg0 <> " (" <> s <> ")"
        in case reads @(PP t x) s of
           [(b,"")] -> mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showLit opts " | " s] [hh pp]
           _ -> mkNode opts (FailT (msg1 <> " failed")) [msg1 <> " failed"] [hh pp]

-- | similar to 'minimum'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Min [10,4,5,12,3,4]
--   Present 3
--   PresentT 3
--
--   >>> pl @Min []
--   Error empty list
--   FailT "empty list"
--
data Min

instance (Ord a, Show a) => P Min [a] where
  type PP Min [a] = a
  eval _ opts as' =
     pure $ case as' of
       [] -> mkNode opts (FailT "empty list") ["Min(empty list)"] []
       as@(_:_) ->
         let v = minimum as
         in mkNode opts (PresentT v) ["Min" <> show0 opts " " v <> showA opts " | " as] []

-- | similar to 'maximum'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Max [10,4,5,12,3,4]
--   Present 12
--   PresentT 12
--
--   >>> pl @Max []
--   Error empty list
--   FailT "empty list"
--

data Max
type Max' t = FoldMap (SG.Max t) Id

instance (Ord a, Show a) => P Max [a] where
  type PP Max [a] = a
  eval _ opts as' =
    pure $ case as' of
      [] -> mkNode opts (FailT "empty list") ["Max(empty list)"] []
      as@(_:_) ->
        let v = maximum as
        in mkNode opts (PresentT v) ["Max" <> show0 opts " " v <> showA opts " | " as] []

-- | sort a list
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(SortOn Fst Id) [(10,"abc"), (3,"def"), (4,"gg"), (10,"xyz"), (1,"z")]
--   Present [(1,"z"),(3,"def"),(4,"gg"),(10,"abc"),(10,"xyz")]
--   PresentT [(1,"z"),(3,"def"),(4,"gg"),(10,"abc"),(10,"xyz")]
--
data SortBy p q
type SortOn p q = SortBy (OrdA p) q
type SortOnDesc p q = SortBy (Swap >> OrdA p) q

type SortByHelper p = Partition (p >> Id == 'GT) Id

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
                [] -> pure $ mkNode opts mempty [msg0 <> " empty"] []
                [w] -> pure $ mkNode opts (PresentT [w]) [msg0 <> " one element " <> show w] []
                w:ys@(_:_) -> do
                  pp <- (if oDebug opts >= 3 then
                              eval (Proxy @(SortByHelper p))
                         else eval (Proxy @(Hide (SortByHelper p)))) opts (map (w,) ys)
--                  pp <- eval (Proxy @(Hide (Partition (p >> Id == 'GT) Id))) opts (map (w,) ys)
-- too much output: dont need (Map Snd *** Map Snd) -- just do map snd in code
--                  pp <- eval (Proxy @(Partition (p >> (Id == 'GT)) Id >> (Map Snd *** Map Snd))) opts (map (w,) ys)
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
          Left _e -> ret -- dont rewrap the error
          Right xs -> mkNode opts (_tBool ret) [msg0 <> show0 opts " " xs] [hh qq, hh ret]

-- | similar to 'length'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Len [10,4,5,12,3,4]
--   Present 6
--   PresentT 6
--
--   >>> pl @Len []
--   Present 0
--   PresentT 0
--
data Len
instance (Show a, as ~ [a]) => P Len as where
  type PP Len as = Int
  eval _ opts as =
    let n = length as
    in pure $ mkNode opts (PresentT n) ["Len" <> show0 opts " " n <> showA opts " | " as] []

-- | similar to 'length' for 'Foldable' instances
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Length Id) (Left "aa")
--   Present 0
--   PresentT 0
--
--   >>> pl @(Length Id) (Right "aa")
--   Present 1
--   PresentT 1
--
--   >>> pl @(Length (Right' Id)) (Right "abcd")
--   Present 4
--   PresentT 4
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
      Right as ->
        let n = length as
        in mkNode opts (PresentT n) [msg0 <> show0 opts " " n <> showA opts " | " as] []

-- | similar to 'Control.Lens._1'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FstL _ Id) (10,"Abc")
--   Present 10
--   PresentT 10
--
data FstL' t p
type FstL (t :: Type) p = FstL' (Hole t) p

instance (PP p x ~ s
        , P p x
        , Show s
        , Field1 s s (PP t x) (PP t x)
        , Show (PP t x)
        ) => P (FstL' t p) x where
  type PP (FstL' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "FstL"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let a = p ^. _1
        in mkNode opts (PresentT a) [msg0 <> show0 opts " " a <> showA opts " | " p] [hh pp]

-- | similar to 'Control.Lens._2'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(SndL _ Id) (10,"Abc")
--   Present "Abc"
--   PresentT "Abc"
--
data SndL' t p
type SndL (t :: Type) p = SndL' (Hole t) p

instance (PP p x ~ s
        , P p x
        , Show s
        , Field2 s s (PP t x) (PP t x)
        , Show (PP t x)
        ) => P (SndL' t p) x where
  type PP (SndL' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "SndL"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let a = p ^. _2
        in mkNode opts (PresentT a) [msg0 <> show0 opts " " a <> showA opts " s=" p] [hh pp]

-- | similar to 'Control.Lens._3'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ThdL _ Id) (10,"Abc",'x')
--   Present 'x'
--   PresentT 'x'
--
data ThdL' t p
type ThdL (t :: Type) p = ThdL' (Hole t) p

instance (PP p x ~ s
        , P p x
        , Show s
        , Field3 s s (PP t x) (PP t x)
        , Show (PP t x)
        ) => P (ThdL' t p) x where
  type PP (ThdL' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "ThdL"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let a = p ^. _3
        in mkNode opts (PresentT a) [msg0 <> show0 opts " " a <> showA opts " | " p] [hh pp]

-- since we support '(,,,) so have to support Field4 to be able to retrieve those values

-- | similar to 'Control.Lens._4'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FthL _ Id) (10,"Abc",'x',True)
--   Present True
--   PresentT True
--
data FthL' t p
type FthL (t :: Type) p = FthL' (Hole t) p

instance (PP p x ~ s
        , P p x
        , Show s
        , Field4 s s (PP t x) (PP t x)
        , Show (PP t x)
        ) => P (FthL' t p) x where
  type PP (FthL' t p) x = PP t x
  eval _ opts x = do
    let msg0 = "FthL"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let a = p ^. _4
        in mkNode opts (PresentT a) [msg0 <> show0 opts " " a <> showA opts " | " p] [hh pp]

-- | similar to 'fst'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Fst (10,"Abc")
--   Present 10
--   PresentT 10
--
data Fst

instance (Show x, Show a) => P Fst (a,x) where
  type PP Fst (a,x) = a
  eval _ opts (a,x) =
    pure $ mkNode opts (PresentT a) ["Fst" <> show0 opts " " a <> showA opts " | " (a,x)] []

-- | similar to 'snd'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Snd (10,"Abc")
--   Present "Abc"
--   PresentT "Abc"
--
data Snd

instance (Show x, Show b) => P Snd (x,b) where
  type PP Snd (x,b) = b
  eval _ opts (x,b) =
    pure $ mkNode opts (PresentT b) ["Snd" <> show0 opts " " b <> showA opts " | " (x,b)] []


-- | 'fst' for a 3-tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Fst3 (10,"Abc",True)
--   Present 10
--   PresentT 10
--
data Fst3

instance (Show x, Show y, Show a) => P Fst3 (a,x,y) where
  type PP Fst3 (a,x,y) = a
  eval _ opts (a,x,y) =
    pure $ mkNode opts (PresentT a) ["Fst3" <> show0 opts " " a <> showA opts " | " (a,x,y)] []

-- | 'snd' for a 3-tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Snd3 (10,"Abc",True)
--   Present "Abc"
--   PresentT "Abc"
--
data Snd3

instance (Show x, Show y, Show b) => P Snd3 (x,b,y) where
  type PP Snd3 (x,b,y) = b
  eval _ opts (x,b,y) =
    pure $ mkNode opts (PresentT b) ["Snd3" <> show0 opts " " b <> showA opts " | " (x,b,y)] []

-- | access to third element in a 3-tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Thd3 (10,True,"Abc")
--   Present "Abc"
--   PresentT "Abc"
--
data Thd3

instance (Show x, Show y, Show b) => P Thd3 (x,y,b) where
  type PP Thd3 (x,y,b) = b
  eval _ opts (x,y,b) =
    pure $ mkNode opts (PresentT b) ["Thd3" <> show0 opts " " b <> showA opts " | " (x,y,b)] []

-- | identity function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @I 23
--   Present 23
--   PresentT 23
data I
instance P I a where
  type PP I a = a
  eval _ opts a =
    pure $ mkNode opts (PresentT a) ["I"] []


-- | identity function that displays the input
--
--   even more constraints than 'I' so we might need to add explicit type signatures
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Id 23
--   Present 23
--   PresentT 23
data Id -- showable version of I
instance Show a => P Id a where
  type PP Id a = a
  eval _ opts a = pure $ mkNode opts (PresentT a) ["Id" <> show0 opts " " a] []


-- | identity function that also displays the type information for debugging
--
--   even more constraints than 'Id' so we might need to explicitly add types (Typeable)
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @IdT 23
--   Present 23
--   PresentT 23
data IdT
instance (Typeable a, Show a) => P IdT a where
  type PP IdT a = a
  eval _ opts a =
    let t = showT @a
    in pure $ mkNode opts (PresentT a) ["IdT(" <> t <> ")" <> show0 opts " " a] []

-- | 'fromString' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :set -XOverloadedStrings
--   >>> pl @(FromStringP (Identity _) Id) "abc"
--   Present Identity "abc"
--   PresentT (Identity "abc")
--
--   >>> pl @(FromStringP (Seq.Seq _) Id) "abc"
--   Present fromList "abc"
--   PresentT (fromList "abc")
data FromStringP' t s
type FromStringP (t :: Type) p = FromStringP' (Hole t) p

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


-- | 'fromInteger' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FromInteger (SG.Sum _) Id) 23
--   Present Sum {getSum = 23}
--   PresentT (Sum {getSum = 23})
data FromInteger' t n
type FromInteger (t :: Type) p = FromInteger' (Hole t) p
type FromIntegerP n = FromInteger' Unproxy n

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

-- | 'fromIntegral' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FromIntegral (SG.Sum _) Id) 23
--   Present Sum {getSum = 23}
--   PresentT (Sum {getSum = 23})
data FromIntegral' t n
type FromIntegral (t :: Type) p = FromIntegral' (Hole t) p

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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " n] [hh nn]

-- | 'toRational' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ToRational Id) 23.5
--   Present 47 % 2
--   PresentT (47 % 2)

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
        in mkNode opts (PresentT r) [msg0 <> show0 opts " " r <> showA opts " | " a] [hh pp]

-- | 'fromRational' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(FromRational Rational Id) 23.5
--   Present 47 % 2
--   PresentT (47 % 2)
data FromRational' t r
type FromRational (t :: Type) p = FromRational' (Hole t) p

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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " r] [hh rr]

-- | 'truncate' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Truncate Int Id) (23 % 5)
--   Present 4
--   PresentT 4
data Truncate' t p
type Truncate (t :: Type) p = Truncate' (Hole t) p

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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " p] [hh pp]

-- | 'ceiling' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Ceiling Int Id) (23 % 5)
--   Present 5
--   PresentT 5
data Ceiling' t p
type Ceiling (t :: Type) p = Ceiling' (Hole t) p

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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " p] [hh pp]

-- | 'floor' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Floor Int Id) (23 % 5)
--   Present 4
--   PresentT 4
data Floor' t p
type Floor (t :: Type) p = Floor' (Hole t) p

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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " p] [hh pp]

-- Start non-Type kinds
-----------------------
-----------------------
-----------------------

-- | pulls the type level 'Bool' to the value level
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'True "ignore this"
--   True
--   TrueT
--
--   >>> pl @'False ()
--   False
--   FalseT
instance GetBool b => P (b :: Bool) a where
  type PP b a = Bool
  eval _ opts _ =
    let b = getBool @b
    in pure $ mkNodeB opts b ["'" <> show b] []

-- | pulls the type level 'Symbol' to the value level
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @"hello world" ()
--   Present "hello world"
--   PresentT "hello world"
instance KnownSymbol s => P (s :: Symbol) a where
  type PP s a = String
  eval _ opts _ =
    let s = symb @s
    in pure $ mkNode opts (PresentT s) ["'" <> showLit0 opts "" s] []

-- | run the predicates in a promoted 2-tuple; similar to 'Control.Arrow.&&&'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'(Snd, Fst) ("helo",123)
--   Present (123,"helo")
--   PresentT (123,"helo")
--
--   >>> :set -XTypeOperators
--   >>> pl @'(Len, Id <> "|" <> Reverse) "helo"
--   Present (4,"helo|oleh")
--   PresentT (4,"helo|oleh")
instance (P p a, P q a) => P '(p,q) a where
  type PP '(p,q) a = (PP p a, PP q a)
  eval _ opts a = do
    let msg = "'(,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
       Left e -> e
       Right (p,q,pp,qq) ->
         mkNode opts (PresentT (p,q)) [msg] [hh pp, hh qq]

-- | run the predicates in a promoted 3-tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'(Len, Id, Reverse) "helo"
--   Present (4,"helo","oleh")
--   PresentT (4,"helo","oleh")
instance (P p a
        , P q a
        , P r a
        ) => P '(p,q,r) a where
  type PP '(p,q,r) a = (PP p a, PP q a, PP r a)
  eval _ opts a = do
    let msg = "'(,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
         let hhs = [hh pp, hh qq]
         rr <- eval (Proxy @r) opts a
         pure $ case getValueLR opts msg rr hhs of
           Left e -> e
           Right r -> mkNode opts (PresentT (p,q,r)) [msg] (hhs <> [hh rr])

-- | run the predicates in a promoted 4-tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'(Len, Id, "inj", 999) "helo"
--   Present (4,"helo","inj",999)
--   PresentT (4,"helo","inj",999)
instance (P p a
        , P q a
        , P r a
        , P s a
        ) => P '(p,q,r,s) a where
  type PP '(p,q,r,s) a = (PP p a, PP q a, PP r a, PP s a)
  eval _ opts a = do
    let msg = "'(,,)"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        lr1 <- runPQ msg (Proxy @r) (Proxy @s) opts a
        pure $ case lr1 of
          Left e -> e
          Right (r,s,rr,ss) ->
            mkNode opts (PresentT (p,q,r,s)) [msg] [hh pp, hh qq, hh rr, hh ss]

-- | extracts the value level representation of the promoted 'Ordering'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'LT "not used"
--   Present LT
--   PresentT LT
--
--   >>> pl @'EQ ()
--   Present EQ
--   PresentT EQ
instance GetOrdering cmp => P (cmp :: Ordering) a where
  type PP cmp a = Ordering
  eval _ opts _a =
    let cmp = getOrdering @cmp
        msg = "'" <> show cmp
    in pure $ mkNode opts (PresentT cmp) [msg] []

-- | extracts the value level representation of the type level 'Nat'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @123 ()
--   Present 123
--   PresentT 123
instance KnownNat n => P (n :: Nat) a where
  type PP n a = Int
  eval _ opts _ =
    let n = nat @n
    in pure $ mkNode opts (PresentT n) ["'" <> show n] []

-- | extracts the value level representation of the type level \'()
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'() ()
--   Present ()
--   PresentT ()
instance P '() a where
  type PP '() a = ()
  eval _ opts _ = pure $ mkNode opts (PresentT ()) ["'()"] []

-- todo: the type has to be [a] so we still need type PP '[p] a = [PP p a] to keep the types in line

-- | extracts the value level representation of the type level \'[]
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'[] False
--   Present []
--   PresentT []
instance P ('[] :: [k]) a where
  type PP ('[] :: [k]) a = [a]
  eval _ opts _ = pure $ mkNode opts mempty ["'[]"] []

-- | runs each predicate in turn from the promoted list
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :set -XNoStarIsType
--   >>> pl @'[1, 2, 3] 999
--   Present [1,2,3]
--   PresentT [1,2,3]
--
--   >>> pl @'[W 1, W 2, W 3, Id] 999
--   Present [1,2,3,999]
--   PresentT [1,2,3,999]
--
--   >>> pl @'[W 1, W 2, W 3, Id * 4, Pred] 999
--   Present [1,2,3,3996,998]
--   PresentT [1,2,3,3996,998]
--
--   >>> :set -XTypeOperators
--   >>> pl @'[Id * 4, Pred] 999
--   Present [3996,998]
--   PresentT [3996,998]
instance (Show (PP p a), Show a, P p a) => P '[p] a where
  type PP '[p] a = [PP p a]
  eval _ opts a = do
    pp <- eval (Proxy @p) opts a
    let msg = "" -- "'[](end)"
    pure $ case getValueLR opts msg pp [] of
       Left e -> e
       Right b -> mkNode opts (PresentT [b]) [msg <> show0 opts " " b <> showA opts " | " a] [hh pp] --  <> show0 opts " " a <> showA opts " b=" b]) [hh pp]

instance (Show (PP p a)
        , Show a
        , P (p1 ': ps) a
        , PP (p1 ': ps) a ~ [PP p1 a]
        , P p a
        , PP p a ~ PP p1 a
        ) => P (p ': p1 ': ps) a where
  type PP (p ': p1 ': ps) a = [PP p a]
  eval _ opts a = do
    let msg = "'"
        -- len = 2 + getLen @ps
    lr <- runPQ msg (Proxy @p) (Proxy @(p1 ': ps)) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        mkNode opts (PresentT (p:q)) [msg <> show0 opts "" (p:q) <> showA opts " | " a] [hh pp, hh qq]

-- | extracts the \'a\' from type level \'Maybe a\' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('Just Id) (Just 123)
--   Present 123
--   PresentT 123
--
--   >>> pl @('Just Not) (Just True)
--   Present False
--   PresentT False
--
--   >>> pl @('Just Id) Nothing
--   Error 'Just found Nothing
--   FailT "'Just found Nothing"
--
instance (Show (PP p a)
        , P p a
        , Show a
        ) => P ('Just p) (Maybe a) where
  type PP ('Just p) (Maybe a) = PP p a
  eval _ opts ma = do
    let msg = "'Just"
    case ma of
      Just a -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msg pp [] of
          Left e -> e
          Right b -> mkNode opts (PresentT b) [msg <> show0 opts " " b <> showA opts " | " ma] [hh pp]
      Nothing -> pure $ mkNode opts (FailT (msg <> " found Nothing")) [msg <> " found Nothing"] []

-- | expects Nothing otherwise it fails
--   if the value is Nothing then it returns \'Proxy a\' as this provides more information than '()'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'Nothing Nothing
--   Present Proxy
--   PresentT Proxy
--
--   >>> pl @'Nothing (Just True)
--   Error 'Nothing found Just
--   FailT "'Nothing found Just"
--
instance P 'Nothing (Maybe a) where
  type PP 'Nothing (Maybe a) = Proxy a -- () gives us less information
  eval _ opts ma =
    let msg = "'Nothing"
    in pure $ case ma of
         Nothing -> mkNode opts (PresentT Proxy) [msg] []
         Just _ -> mkNode opts (FailT (msg <> " found Just")) [msg <> " found Just"] []

-- omitted Show x so we can have less ambiguity
-- | extracts the \'a\' from type level \'Either a b\' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('Left Id) (Left 123)
--   Present 123
--   PresentT 123
--
--   >>> pl @('Left Id) (Right "aaa")
--   Error 'Left found Right
--   FailT "'Left found Right"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('Left p) (Either a x) where
  type PP ('Left p) (Either a x) = PP p a
  eval _ opts lr =
    let msg = "'Left"
    in case lr of
         Right _ -> pure $ mkNode opts (FailT (msg <> " found Right")) [msg <> " found Right"] []
         Left a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | Left " a] [hh pp]

-- | extracts the \'b\' from type level \'Either a b\' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('Right Id) (Right 123)
--   Present 123
--   PresentT 123
--
--   >>> pl @('Right Id) (Left "aaa")
--   Error 'Right found Left
--   FailT "'Right found Left"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('Right p) (Either x a) where
  type PP ('Right p) (Either x a) = PP p a
  eval _ opts lr = do
    let msg = "'Right"
    case lr of
         Left _ -> pure $ mkNode opts (FailT (msg <> " found Left")) [msg <> " found Left"] []
         Right a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | Right " a] [hh pp]

-- removed Show x: else ambiguity errors in TestPredicate

-- | extracts the \'a\' from type level \'These a b\' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('This Id) (This 123)
--   Present 123
--   PresentT 123
--
--   >>> pl @('This Id) (That "aaa")
--   Error 'This found That
--   FailT "'This found That"
--
--   >>> pl @('This Id) (These 999 "aaa")
--   Error 'This found These
--   FailT "'This found These"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('This p) (These a x) where
  type PP ('This p) (These a x) = PP p a
  eval _ opts th = do
    let msg = "'This"
    case th of
         This a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | This " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []

-- | extracts the \'b\' from type level \'These a b\' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('That Id) (That 123)
--   Present 123
--   PresentT 123
--
--   >>> pl @('That Id) (This "aaa")
--   Error 'That found This
--   FailT "'That found This"
--
--   >>> pl @('That Id) (These 44 "aaa")
--   Error 'That found These
--   FailT "'That found These"
--
instance (Show a
        , Show (PP p a)
        , P p a
        ) => P ('That p) (These x a) where
  type PP ('That p) (These x a) = PP p a
  eval _ opts th = do
    let msg = "'That"
    case th of
         That a -> do
            pp <- eval (Proxy @p) opts a
            pure $ case getValueLR opts msg pp [] of
                 Left e -> e
                 Right b -> mkNode opts (_tBool pp) [msg <> show0 opts " " b <> showA opts " | That " a] [hh pp]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []


-- | extracts the (a,b) from type level 'These a b' if the value exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @('These Id Id) (These 123 "abc")
--   Present (123,"abc")
--   PresentT (123,"abc")
--
--   >>> pl @('These Pred Len) (These 123 "abcde")
--   Present (122,5)
--   PresentT (122,5)
--
--   >>> pl @('These Id Id) (This "aaa")
--   Error 'These found This
--   FailT "'These found This"
--
--   >>> pl @('These Id Id) (That "aaa")
--   Error 'These found That
--   FailT "'These found That"
--
instance (Show a
        , Show b
        , P p a
        , P q b
        , Show (PP p a)
        , Show (PP q b)
        ) => P ('These p q) (These a b) where
  type PP ('These p q) (These a b) = (PP p a, PP q b)
  eval _ opts th = do
    let msg = "'These"
    case th of
         These a b -> do
            pp <- eval (Proxy @p) opts a
            case getValueLR opts msg pp [] of
               Left e -> pure e
               Right p -> do
                 qq <- eval (Proxy @q) opts b
                 pure $ case getValueLR opts (msg <> " q failed p=" <> show p) qq [hh pp] of
                    Left e -> e
                    Right q -> mkNode opts (PresentT (p,q)) [msg <> show0 opts " " (p,q) <> showA opts " | " (These a b)] [hh pp, hh qq]
         _ -> pure $ mkNode opts (FailT (msg <> " found " <> showThese th)) [msg <> " found " <> showThese th] []

-- | converts the value to the corresponding 'Proxy'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @'Proxy 'x'
--   Present Proxy
--   PresentT Proxy
--
instance Show a => P 'Proxy a where
  type PP 'Proxy a = Proxy a
  eval _ opts a =
    let b = Proxy @a
    in pure $ mkNode opts (PresentT b) ["'Proxy" <> showA opts " | " a] []

-- End non-Type kinds
-----------------------
-----------------------
-----------------------

-- | converts a value to a 'Proxy': the same as '\'Proxy'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @MkProxy 'x'
--   Present Proxy
--   PresentT Proxy
--
data MkProxy

instance Show a => P MkProxy a where
  type PP MkProxy a = Proxy a
  eval _ opts a =
    let b = Proxy @a
    in pure $ mkNode opts (PresentT b) ["MkProxy" <> showA opts " | " a] []

type family DoExpandT (ps :: [k]) :: Type where
  DoExpandT '[] = GL.TypeError ('GL.Text "'[] invalid: requires at least one predicate in the list")
  DoExpandT '[p] = Id >> p -- need this else fails cos 1 is nat and would mean that the result is nat not Type!
  -- if p >> Id then turns TrueT to PresentT True
  DoExpandT (p ': p1 ': ps) = p >> DoExpandT (p1 ': ps)

-- | processes a type level list predicates running each in sequence: see 'Predicate.>>'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Do [Pred, ShowP, Id &&& Len]) 9876543
--   Present ("9876542",7)
--   PresentT ("9876542",7)
--
data Do (ps :: [k])
instance (P (DoExpandT ps) a) => P (Do ps) a where
  type PP (Do ps) a = PP (DoExpandT ps) a
  eval _ = eval (Proxy @(DoExpandT ps))

-- | Convenient method to convert a value \'p\' to a 'Maybe' based on a predicate '\b\'
--   if '\b\' then Just \'p'\ else Nothing
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MaybeB (Id > 4) Id) 24
--   Present Just 24
--   PresentT (Just 24)
--
--   >>> pl @(MaybeB (Id > 4) Id) (-5)
--   Present Nothing
--   PresentT Nothing
--
data MaybeB b p

instance (Show (PP p a)
        , P b a
        , P p a
        , PP b a ~ Bool
        ) => P (MaybeB b p) a where
  type PP (MaybeB b p) a = Maybe (PP p a)
  eval _ opts z = do
    let msg0 = "MaybeB"
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
--   if \'b\' then Right \'p\' else Left '\q\'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(EitherB (Fst > 4) (Snd >> Fst) (Snd >> Snd)) (24,(-1,999))
--   Present Right 999
--   PresentT (Right 999)
--
--   >>> pl @(EitherB (Fst > 4) (Snd >> Fst) (Snd >> Snd)) (1,(-1,999))
--   Present Left (-1)
--   PresentT (Left (-1))
--
data EitherB b p q

instance (Show (PP p a)
        , P p a
        , Show (PP q a)
        , P q a
        , P b a
        , PP b a ~ Bool
        ) => P (EitherB b p q) a where
  type PP (EitherB b p q) a = Either (PP p a) (PP q a)
  eval _ opts z = do
    let msg0 = "EitherB"
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
        pure $ case getValueLR opts (msg0 <> " p failed") qq [hh bb] of
          Left e -> e
          Right q -> mkNode opts (PresentT (Right q)) [msg0 <> "(True)" <> show0 opts " Right " q] [hh bb, hh qq]

-- | create inductive tuples from a type level list of predicates
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(TupleI '[Id,ShowP,Pred,W "str", W 999]) 666
--   Present (666,("666",(665,("str",(999,())))))
--   PresentT (666,("666",(665,("str",(999,())))))
--
data TupleI (ps :: [k]) -- make it an inductive tuple

instance P (TupleI ('[] :: [k])) a where
  type PP (TupleI ('[] :: [k])) a = ()
  eval _ opts _ = pure $ mkNode opts (PresentT ()) ["TupleI(done)"] []

instance (P p a
        , P (TupleI ps) a
        , Show a
        ) => P (TupleI (p ': ps)) a where
  type PP (TupleI (p ': ps)) a = (PP p a, PP (TupleI ps) a)
  eval _ opts a = do
    pp <- eval (Proxy @p) opts a
    let msg = "TupleI" -- "'[](" <> show len <> ")"
    case getValueLR opts msg pp [] of
         Left e -> pure e
         Right w -> do
           qq <- eval (Proxy @(TupleI ps)) opts a
           pure $ case getValueLR opts msg qq [hh pp] of
                Left e -> e
                -- only PresentP makes sense here (ie not TrueP/FalseP: ok in base case tho
                Right ws -> mkNode opts (PresentT (w,ws)) [msg <> show0 opts " " a] [hh pp, hh qq]

-- | type level representation of signed rational numbers/integers
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :set -XNoStarIsType
--   >>> pl @(NegR 14 3) ()
--   Present (-14) % 3
--   PresentT ((-14) % 3)
--
--   >>> pl @(PosR 14 3) ()
--   Present 14 % 3
--   PresentT (14 % 3)
--
--   >>> pl @(CmpRat (NegR 14 3) (Neg 5)) ()
--   Present GT
--   PresentT GT
--
--   >>> pl @(NegR 14 3 * Neg 5) ()
--   Present 70 % 3
--   PresentT (70 % 3)
--
--   >>> pl @(NegR 14 3 - Pos 5) ()
--   Present (-29) % 3
--   PresentT ((-29) % 3)
--
--   >>> pl @(CmpRat (PosR 14 3) 5) ()
--   Present LT
--   PresentT LT

data Rat (pos :: Bool) (num :: Nat) (den :: Nat)
-- | constructs a positive integer as a rational number 'Rat'
type Pos (n :: Nat) = Rat 'True n 1
-- | constructs a negative integer as a rational number 'Rat'
type Neg (n :: Nat) = Rat 'False n 1

-- | constructs a valid positive rational number 'Rat'
type family PosR (n :: Nat) (d :: Nat) where
  PosR n 0 = GL.TypeError ('GL.Text "PosR has a 0 denominator where numerator=" ':<>: 'GL.ShowType n)
  PosR n d = Rat 'True n d

-- | constructs a valid negative rational number 'Rat'
type family NegR (n :: Nat) (d :: Nat) where
  NegR n 0 = GL.TypeError ('GL.Text "NegR has a 0 denominator where numerator=" ':<>: 'GL.ShowType n)
  NegR n d = Rat 'False n d

instance (GetBool pos
        , KnownNat num
        , KnownNat den
        , NotZeroT den
        ) => P (Rat pos num den) a where
  type PP (Rat pos num den) a = Rational
  eval _ opts _ =
    let pos = getBool @pos
        num = nat @num
        den = nat @den
        msg = "Rat " <> show r
        r = (if pos then id else negate) (num % den)
    in pure $ mkNode opts (PresentT r) [msg] []

-- | compares 2 numbers where the numbers are type level signed rationals or Nats
type family CmpRat (m :: k) (n :: k1) :: Ordering where
  CmpRat (Rat x n 0) z = GL.TypeError ('GL.Text "CmpRat: lhs has 0 denominator" ':$$: 'GL.ShowType (Rat x n 0) ':<>: 'GL.Text " `CmpRat` " ':<>: 'GL.ShowType z)
  CmpRat z (Rat x n 0) = GL.TypeError ('GL.Text "CmpRat: rhs has 0 denominator" ':$$: 'GL.ShowType z ':<>: 'GL.Text " `CmpRat` " ':<>: 'GL.ShowType (Rat x n 0))
  CmpRat (m :: Nat) (n :: Nat) = GN.CmpNat m n
  CmpRat (Rat x n d) (w :: Nat) = CmpRat (Rat x n d) (Pos w)
  CmpRat (w :: Nat) (Rat x n d) = CmpRat (Pos w) (Rat x n d)
  CmpRat (Rat x 0 d) (Rat x1 0 d1) = 'EQ
  CmpRat (Rat 'True n d) (Rat 'False n1 d1) = 'GT
  CmpRat (Rat 'False n d) (Rat 'True n1 d1) = 'LT
  CmpRat (Rat 'False n d) (Rat 'False n1 d1) =
    CmpRat (Rat 'True n1 d1) (Rat 'True n d)
  CmpRat (Rat 'True n d) (Rat 'True n1 d1) =
    IfT (GN.CmpNat (GN.Div n d) (GN.Div n1 d1) DE.== 'EQ)
       (GN.CmpNat (n GN.* d1) (n1 GN.* d))
       (GN.CmpNat (GN.Div n d) (GN.Div n1 d1))

-- | get a list of 'Rational's from the type level
class GetRats as where
  getRats :: [Rational]
instance GetRats '[] where
  getRats = []
instance (GetRat n, GetRats ns) => GetRats (n ': ns) where
  getRats = getRat @n : getRats @ns

-- | get a 'Rational' from the type level
class GetRat a where
  getRat :: Rational
instance KnownNat n => GetRat (n :: Nat) where
  getRat = nat @n
instance (GetBool pos, KnownNat num, KnownNat den, NotZeroT den) => GetRat (Rat (pos :: Bool) (num :: Nat) (den :: Nat)) where
  getRat = let s = getBool @pos
               n = nat @num
               d = nat @den
           in (if s then 1 else (-1)) * n % d

-- | add a message to give more context to the evaluation tree
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pe @(Msg' "somemessage" Id) 999
--   P [somemessage] Id 999
--   PresentT 999
--
data Msg prt p
type Msg' prt p = Msg (Printf "[%s] " prt) p -- put msg in square brackets

instance (P prt a
        , PP prt a ~ String
        , P p a
        ) => P (Msg prt p) a where
  type PP (Msg prt p) a = PP p a
  eval _ opts a = do
    pp <- eval (Proxy @prt) opts a
    case getValueLR opts "Msg" pp [] of
         Left e -> pure e
         Right msg -> prefixMsg msg <$> eval (Proxy @p) opts a

-- | pad \'q\' with '\n'\ values from '\p'\
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(PadL 5 999 Id) [12,13]
--   Present [999,999,999,12,13]
--   PresentT [999,999,999,12,13]
--
--   >>> pl @(PadR 5 Fst '[12,13]) (999,'x')
--   Present [12,13,999,999,999]
--   PresentT [12,13,999,999,999]
--
--   >>> pl @(PadR 2 Fst '[12,13,14]) (999,'x')
--   Present [12,13,14]
--   PresentT [12,13,14]
--
data Pad (left :: Bool) n p q
type PadL n p q = Pad 'True n p q
type PadR n p q = Pad 'False n p q

instance (P n a
        , GetBool left
        , Integral (PP n a)
        , [PP p a] ~ PP q a
        , P p a
        , P q a
        , Show (PP p a)
        ) => P (Pad left n p q) a where
  type PP (Pad left n p q) a = PP q a
  eval _ opts a = do
    let msg0 = "Pad" <> (if lft then "L" else "R")
        lft = getBool @left
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a
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
            in mkNode opts (PresentT bs) [msg1 <> show0 opts " " bs <> showA opts " | " q] (hhs <> [hh qq])

-- | split a list \'p\' into parts using the lengths in the type level list \'ns\'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(SplitAts '[2,3,1,1] Id) "hello world"
--   Present ["he","llo"," ","w","orld"]
--   PresentT ["he","llo"," ","w","orld"]
--
--   >>> pl @(SplitAts '[2] Id) "hello world"
--   Present ["he","llo world"]
--   PresentT ["he","llo world"]
--
--   >>> pl @(SplitAts '[10,1,1,5] Id) "hello world"
--   Present ["hello worl","d","",""]
--   PresentT ["hello worl","d","",""]
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
    let msg = "SplitAts"
    lr <- runPQ msg (Proxy @ns) (Proxy @p) opts x
    pure $ case lr of
      Left e -> e
      Right (ns,p,nn,pp) ->
        let zs = foldr (\n k s -> let (a,b) = splitAt (fromIntegral n) s
                              in a:k b
                   ) (\as -> if null as then [] else [as]) ns p
        in mkNode opts (PresentT zs) [msg <> show0 opts " " zs <> showA opts " | ns=" ns <> showA opts " | " p] [hh nn, hh pp]

-- | similar to 'splitAt'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(SplitAt 4 Id) "hello world"
--   Present ("hell","o world")
--   PresentT ("hell","o world")
--
--   >>> pl @(SplitAt 20 Id) "hello world"
--   Present ("hello world","")
--   PresentT ("hello world","")
--
--   >>> pl @(SplitAt 0 Id) "hello world"
--   Present ("","hello world")
--   PresentT ("","hello world")
--
--   >>> pl @(SplitAt Snd Fst) ("hello world",4)
--   Present ("hell","o world")
--   PresentT ("hell","o world")
--
data SplitAt n p
type Take n p = SplitAt n p >> Fst
type Drop n p = SplitAt n p >> Snd

instance (PP p a ~ [b]
        , P n a
        , P p a
        , Show b
        , Integral (PP n a)
        ) => P (SplitAt n p) a where
  type PP (SplitAt n p) a = (PP p a, PP p a)
  eval _ opts a = do
    let msg0 = "SplitAt"
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a
    pure $ case lr of
      Left e -> e -- (Left e, tt')
      Right (fromIntegral -> n,p,pp,qq) ->
        let msg1 = msg0 <> show0 opts " " n <> show0 opts " " p
            (x,y) = splitAt n p
       in mkNode opts (PresentT (x,y)) [msg1 <> show0 opts " " (x,y) <> showA opts " | n=" n <> showA opts " | " p] [hh pp, hh qq]

type Tail = Uncons >> 'Just Snd
type Head = Uncons >> 'Just Fst
type Init = Unsnoc >> 'Just Fst
type Last = Unsnoc >> 'Just Snd

-- | similar to 'Control.Arrow.&&&'
type p &&& q = W '(p, q)
infixr 3 &&&

-- | similar to 'Control.Arrow.***'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Pred *** ShowP) (13, True)
--   Present (12,"True")
--   PresentT (12,"True")
--
data (p :: k) *** (q :: k1)
type Star p q = p *** q
infixr 3 ***
type First p = Star p I
type Second q = Star I q

instance (Show (PP p a)
        , Show (PP q b)
        , P p a
        , P q b
        , Show a
        , Show b
        ) => P (p *** q) (a,b) where
  type PP (p *** q) (a,b) = (PP p a, PP q b)
  eval _ opts (a,b) = do
    let msg = "(***)"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg pp [] of
      Left e -> pure e
      Right a1 -> do
        qq <- eval (Proxy @q) opts b
        pure $ case getValueLR opts msg qq [hh pp] of
          Left e -> e
          Right b1 -> mkNode opts (PresentT (a1,b1)) [msg <> show0 opts " " (a1,b1) <> showA opts " | " (a,b)] [hh pp, hh qq]

-- | similar 'Control.Arrow.|||'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Pred ||| Id) (Left 13)
--   Present 12
--   PresentT 12
--
--   >>> pl @(ShowP ||| Id) (Right "hello")
--   Present "hello"
--   PresentT "hello"
--
data (|||) (p :: k) (q :: k1)
infixr 2 |||
type EitherIn p q = p ||| q
type IsLeft = 'True ||| 'False
type IsRight = 'False ||| 'True

instance (Show (PP p a)
        , P p a
        , P q b
        , PP p a ~ PP q b
        , Show a
        , Show b
        ) => P (p ||| q) (Either a b) where
  type PP (p ||| q) (Either a b) = PP p a
  eval _ opts (Left a) = do
    let msg = "|||"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg pp [] of
      Left e -> e
      Right a1 -> mkNode opts (_tBool pp) ["Left" <> show0 opts " " a1 <> showA opts " | " a] [hh pp]
  eval _ opts (Right a) = do
    let msg = "|||"
    qq <- eval (Proxy @q) opts a
    pure $ case getValueLR opts msg qq [] of
      Left e -> e
      Right a1 -> mkNode opts (_tBool qq) ["Right" <> show0 opts " " a1 <> showA opts " | " a] [hh qq]

-- | similar 'Control.Arrow.+++'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Pred +++ Id) (Left 13)
--   Present Left 12
--   PresentT (Left 12)
--
--   >>> pl @(ShowP +++ Reverse) (Right "hello")
--   Present Right "olleh"
--   PresentT (Right "olleh")
--
data (+++) (p :: k) (q :: k1)
infixr 2 +++

instance (Show (PP p a)
        , Show (PP q b)
        , P p a
        , P q b
        , Show a
        , Show b
        ) => P (p +++ q) (Either a b) where
  type PP (p +++ q) (Either a b) = Either (PP p a) (PP q b)
  eval _ opts (Left a) = do
    let msg = "+++"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts msg pp [] of
      Left e -> e
      Right a1 -> mkNode opts (PresentT (Left a1)) ["(+++) Left" <> show0 opts " Left " a1 <> showA opts " | " a] [hh pp]
  eval _ opts (Right a) = do
    let msg = "+++"
    qq <- eval (Proxy @q) opts a
    pure $ case getValueLR opts msg qq [] of
      Left e -> e
      Right a1 -> mkNode opts (PresentT (Right a1)) ["(+++) Right" <> show0 opts " Right" a1 <> showA opts " | " a] [hh qq]

type Dup = '(Id, Id)

data BinOp = BMult | BSub | BAdd deriving (Show,Eq)

type Mult p q = Bin 'BMult p q
type Add p q = Bin 'BAdd p q
type Sub p q = Bin 'BSub p q

type p + q = Add p q
infixl 6 +
type p - q = Sub p q
infixl 6 -
type p * q = Mult p q
infixl 7 *

type p > q = Cmp 'Cgt p q
infix 4 >
type p >= q = Cmp 'Cge p q
infix 4 >=
type p == q = Cmp 'Ceq p q
infix 4 ==
type p /= q = Cmp 'Cne p q
infix 4 /=
type p <= q = Cmp 'Cle p q
infix 4 <=
type p < q = Cmp 'Clt p q
infix 4 <

type p >? q = CmpI 'Cgt p q
infix 4 >?
type p >=? q = CmpI 'Cge p q
infix 4 >=?
type p ==? q = CmpI 'Ceq p q
infix 4 ==?
type p /=? q = CmpI 'Cne p q
infix 4 /=?
type p <=? q = CmpI 'Cle p q
infix 4 <=?
type p <? q = CmpI 'Clt p q
infix 4 <?

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
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :set -XNoStarIsType
--   >>> pl @(Fst * Snd) (13,5)
--   Present 65
--   PresentT 65
--
--   >>> pl @(Fst + 4 * (Snd >> Len) - 4) (3,"hello")
--   Present 19
--   PresentT 19
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
    lr <- runPQ s (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p `f` q
        in mkNode opts (PresentT d) [show p <> " " <> s <> " " <> show q <> " = " <> show d] [hh pp, hh qq]

-- | fractional division
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst / Snd) (13,2)
--   Present 6.5
--   PresentT 6.5
--
--   >>> pl @(Pos 13 / Id) 0
--   Error DivF zero denominator
--   FailT "DivF zero denominator"
--
data DivF p q
type p / q = DivF p q
infixl 7 /

instance (PP p a ~ PP q a
        , Eq (PP q a)
        , P p a
        , P q a
        , Show (PP p a)
        , Fractional (PP p a)
        ) => P (DivF p q) a where
  type PP (DivF p q) a = PP p a
  eval _ opts a = do
    let msg = "DivF"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)
         | q == 0 -> let msg1 = msg <> " zero denominator"
                     in mkNode opts (FailT msg1) [msg1] [hh pp, hh qq]
         | otherwise ->
            let d = p / q
            in mkNode opts (PresentT d) [show p <> " / " <> show q <> " = " <> show d] [hh pp, hh qq]

-- | similar to 'negate'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Negate 14
--   Present -14
--   PresentT (-14)
--
data Negate

instance (Show a, Num a) => P Negate a where
  type PP Negate a = a
  eval _ opts a =
    let d = negate a
    in pure $ mkNode opts (PresentT d) ["Negate" <> show0 opts " " d <> showA opts " | " a] []

-- | similar to 'abs'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Abs (-14)
--   Present 14
--   PresentT 14
--
--   >>> pl @Abs 14
--   Present 14
--   PresentT 14
--
--   >>> pl @Abs 0
--   Present 0
--   PresentT 0
--
data Abs

instance (Show a, Num a) => P Abs a where
  type PP Abs a = a
  eval _ opts a =
    let d = abs a
    in pure $ mkNode opts (PresentT d) ["Abs" <> show0 opts " " d <> showA opts " | " a] []

-- | similar to 'signum'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Signum (-14)
--   Present -1
--   PresentT (-1)
--
--   >>> pl @Signum 14
--   Present 1
--   PresentT 1
--
--   >>> pl @Signum 0
--   Present 0
--   PresentT 0
--
data Signum

instance (Show a, Num a) => P Signum a where
  type PP Signum a = a
  eval _ opts a =
    let d = signum a
    in pure $ mkNode opts (PresentT d) ["Signum" <> show0 opts " " d <> showA opts " | " a] []

-- | unwraps a value (see 'Control.Lens.Unwrapped')
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Unwrap (SG.Sum (-13))
--   Present -13
--   PresentT (-13)
--
data Unwrap

instance (Show s
        , Show (Unwrapped s)
        , Wrapped s
        ) => P Unwrap s where
  type PP Unwrap s = Unwrapped s
  eval _ opts as =
    let d = as ^. _Wrapped'
    in pure $ mkNode opts (PresentT d) ["Unwrap" <> show0 opts " " d <> showA opts " | " as] []

-- | wraps a value (see 'Control.Lens.Wrapped' and 'Control.Lens.Unwrapped')
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> :m + Data.List.NonEmpty
--   >>> pl @(Wrap (SG.Sum _) Id) (-13)
--   Present Sum {getSum = -13}
--   PresentT (Sum {getSum = -13})
--
--   >>> pl @(Wrap SG.Any (Ge 4)) 13
--   Present Any {getAny = True}
--   PresentT (Any {getAny = True})
--
--   >>> pl @(Wrap (NonEmpty _) (Uncons >> 'Just Id)) "abcd"
--   Present 'a' :| "bcd"
--   PresentT ('a' :| "bcd")
--
data Wrap' t p
type Wrap (t :: Type) p = Wrap' (Hole t) p

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
        in mkNode opts (PresentT d) ["Wrap" <> show0 opts " " d <> showA opts " | " p] [hh pp]

-- | similar to 'coerce'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Coerce (SG.Sum Integer)) (Identity (-13))
--   Present Sum {getSum = -13}
--   PresentT (Sum {getSum = -13})
--
data Coerce (t :: k)

instance (Show a
        , Show t
        , Coercible t a
        ) => P (Coerce t) a where
  type PP (Coerce t) a = t
  eval _ opts a =
    let d = a ^. coerced
    in pure $ mkNode opts (PresentT d) ["Coerce" <> show0 opts " " d <> showA opts " | " a] []

-- can coerce over a functor: but need to provide type of 'a' and 't' explicitly

-- | see 'Coerce': coerce over a functor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Coerce2 (SG.Sum Integer)) [Identity (-13), Identity 4, Identity 99]
--   Present [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
--   PresentT [Sum {getSum = -13},Sum {getSum = 4},Sum {getSum = 99}]
--
--   >>> pl @(Coerce2 (SG.Sum Integer)) (Just (Identity (-13)))
--   Present Just (Sum {getSum = -13})
--   PresentT (Just (Sum {getSum = -13}))
--
--   >>> pl @(Coerce2 (SG.Sum Int)) (Nothing @(Identity Int))
--   Present Nothing
--   PresentT Nothing
--
data Coerce2 (t :: k)
instance (Show (f a)
        , Show (f t)
        , Coercible t a
        , Functor f
        ) => P (Coerce2 t) (f a) where
  type PP (Coerce2 t) (f a) = f t
  eval _ opts fa =
    let d = view coerced <$> fa
    in pure $ mkNode opts (PresentT d) ["Coerce2" <> show0 opts " " d <> showA opts " | " fa] []

-- | lift mempty over a Functor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MemptyT2 (SG.Product Int)) [Identity (-13), Identity 4, Identity 99]
--   Present [Product {getProduct = 1},Product {getProduct = 1},Product {getProduct = 1}]
--   PresentT [Product {getProduct = 1},Product {getProduct = 1},Product {getProduct = 1}]
--
data MemptyT2' t
type MemptyT2 t = MemptyT2' (Hole t)

instance (Show (f a)
        , Show (f (PP t (f a)))
        , Functor f
        , Monoid (PP t (f a))
        ) => P (MemptyT2' t) (f a) where
  type PP (MemptyT2' t) (f a) = f (PP t (f a))
  eval _ opts fa =
    let b = mempty <$> fa
    in pure $ mkNode opts (PresentT b) ["MemptyT2" <> show0 opts " " b <> showA opts " | " fa] []

-- | lift pure over a Functor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Pure2 (Either String)) [1,2,4]
--   Present [Right 1,Right 2,Right 4]
--   PresentT [Right 1,Right 2,Right 4]
--
data Pure2 (t :: Type -> Type)
type Right t = Pure (Either t) Id
type Left t = Right t >> Swap

instance (Show (f (t a))
        , Show (f a)
        , Applicative t
        , Functor f
        ) => P (Pure2 t) (f a) where
  type PP (Pure2 t) (f a) = f (t a)
  eval _ opts fa =
    let b = fmap pure fa
    in pure $ mkNode opts (PresentT b) ["Pure2" <> show0 opts " " b <> showA opts " | " fa] []

-- | similar to 'reverse'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Reverse [1,2,4]
--   Present [4,2,1]
--   PresentT [4,2,1]
--
--   >>> pl @Reverse "AbcDeF"
--   Present "FeDcbA"
--   PresentT "FeDcbA"
--
data Reverse

instance (Show a, as ~ [a]) => P Reverse as where
  type PP Reverse as = as
  eval _ opts as =
    let d = reverse as
    in pure $ mkNode opts (PresentT d) ["Reverse" <> show0 opts " " d <> showA opts " | " as] []

-- | reverses using 'reversing'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> import Data.Text (Text)
--   >>> pl @ReverseL ("AbcDeF" :: Text)
--   Present "FeDcbA"
--   PresentT "FeDcbA"
--
data ReverseL

instance (Show t, Reversing t) => P ReverseL t where
  type PP ReverseL t = t
  eval _ opts as =
    let d = as ^. reversed
    in pure $ mkNode opts (PresentT d) ["ReverseL" <> show0 opts " " d <> showA opts " | " as] []

-- | swaps using 'swapped'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Swap (Left 123)
--   Present Right 123
--   PresentT (Right 123)
--
--   >>> pl @Swap (Right 123)
--   Present Left 123
--   PresentT (Left 123)
--
--   >>> pl @Swap (These 'x' 123)
--   Present These 123 'x'
--   PresentT (These 123 'x')
--
--   >>> pl @Swap (This 'x')
--   Present That 'x'
--   PresentT (That 'x')
--
--   >>> pl @Swap (That 123)
--   Present This 123
--   PresentT (This 123)
--
--   >>> pl @Swap (123,'x')
--   Present ('x',123)
--   PresentT ('x',123)
--
data Swap

instance (Show (p a b)
        , Swapped p
        , Show (p b a)
        ) => P Swap (p a b) where
  type PP Swap (p a b) = p b a
  eval _ opts pab =
    let d = pab ^. swapped
    in pure $ mkNode opts (PresentT d) ["Swap" <> show0 opts " " d <> showA opts " | " pab] []

-- | bounded 'succ' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @SuccB' (13 :: Int)
--   Present 14
--   PresentT 14
--
--   >>> pl @SuccB' LT
--   Present EQ
--   PresentT EQ
--
--   >>> pl @(SuccB 'LT) GT
--   Present LT
--   PresentT LT
--
--   >>> pl @SuccB' GT
--   Error Succ bounded failed
--   FailT "Succ bounded failed"
--
data SuccB def
type SuccB' = SuccB (Failp "Succ bounded failed")

instance (P def (Proxy a)
        , PP def (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (SuccB def) a where
  type PP (SuccB def) a = a
  eval _ opts a = do
    let msg0 = "SuccB"
    case succMay a of
      Nothing -> do
         let msg1 = msg0 <> " out of range"
         pp <- eval (Proxy @def) opts (Proxy @a)
         pure $ case getValueLR opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNode opts (_tBool pp) [msg1] [hh pp]
      Just n -> pure $ mkNode opts (PresentT n) [msg0 <> show0 opts " " n <> showA opts " | " a] []

-- | bounded 'pred' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @PredB' (13 :: Int)
--   Present 12
--   PresentT 12
data PredB def
type PredB' = PredB (Failp "Pred bounded failed")

instance (P def (Proxy a)
        , PP def (Proxy a) ~ a
        , Show a
        , Eq a
        , Bounded a
        , Enum a
        ) => P (PredB def) a where
  type PP (PredB def) a = a
  eval _ opts a = do
    let msg0 = "PredB"
    case predMay a of
      Nothing -> do
         let msg1 = msg0 <> " out of range"
         pp <- eval (Proxy @def) opts (Proxy @a)
         pure $ case getValueLR opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNode opts (_tBool pp) [msg1] [hh pp]
      Just n -> pure $ mkNode opts (PresentT n) [msg0 <> show0 opts " " n <> showA opts " | " a] []

-- | unbounded 'succ' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Succ 13
--   Present 14
--   PresentT 14
--
--   >>> pl @Succ LT
--   Present EQ
--   PresentT EQ
--
--   >>> pl @Succ GT
--   Error Succ IO e=Prelude.Enum.Ordering.succ: bad argument
--   FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument"
--
data Succ
instance (Show a, Enum a) => P Succ a where
  type PP Succ a = a
  eval _ opts a = do
    let msg = "Succ"
    lr <- catchit @_ @E.SomeException (succ a)
    pure $ case lr of
      Left e -> mkNode opts (FailT (msg <> " " <> e)) [msg <> show0 opts " " a] []
      Right n -> mkNode opts (PresentT n) [msg <> show0 opts " " n <> showA opts " | " a] []


-- | unbounded 'pred' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Pred 13
--   Present 12
--   PresentT 12
data Pred
instance (Show a, Enum a) => P Pred a where
  type PP Pred a = a
  eval _ opts a = do
    let msg = "Pred"
    lr <- catchit @_ @E.SomeException (pred a)
    pure $ case lr of
      Left e -> mkNode opts (FailT (msg <> " " <> e)) [msg <> show0 opts " " a] []
      Right n -> mkNode opts (PresentT n) [msg <> show0 opts " " n <> showA opts " | " a] []


-- | 'fromEnum' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @FromEnum 'x'
--   Present 120
--   PresentT 120
data FromEnum
instance (Show a, Enum a) => P FromEnum a where
  type PP FromEnum a = Int
  eval _ opts a =
    let n = fromEnum a
    in pure $ mkNode opts (PresentT n) ["FromEnum" <> show0 opts " " n <> showA opts " | " a] []

-- | unsafe 'toEnum' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ToEnum Char) 120
--   Present 'x'
--   PresentT 'x'
data ToEnum' t
type ToEnum (t :: Type) = ToEnum' (Hole t)

instance (Show a
        , Enum (PP t a)
        , Show (PP t a)
        , Integral a
        ) => P (ToEnum' t) a where
  type PP (ToEnum' t) a = PP t a
  eval _ opts a = do
    let msg = "ToEnum"
    lr <- catchit @_ @E.SomeException (toEnum $! fromIntegral a)
    pure $ case lr of
      Left e -> mkNode opts (FailT (msg <> " " <> e)) [msg <> show0 opts " " a] []
      Right n -> mkNode opts (PresentT n) [msg <> show0 opts " " n <> showA opts " | " a] []

-- | bounded 'toEnum' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ToEnumB Ordering LT) 2
--   Present GT
--   PresentT GT
--
--   >>> pl @(ToEnumB Ordering LT) 6
--   Present LT
--   PresentT LT
--
--   >>> pl @(ToEnumBF Ordering) 6
--   Error ToEnum bounded failed
--   FailT "ToEnum bounded failed"
--
data ToEnumB' t def
type ToEnumB (t :: Type) def = ToEnumB' (Hole t) def
type ToEnumBF (t :: Type) = ToEnumB' (Hole t) (Failp "ToEnum bounded failed")

instance (P def (Proxy (PP t a))
        , PP def (Proxy (PP t a)) ~ (PP t a)
        , Show a
        , Show (PP t a)
        , Bounded (PP t a)
        , Enum (PP t a)
        , Integral a
        ) => P (ToEnumB' t def) a where
  type PP (ToEnumB' t def) a = PP t a
  eval _ opts a = do
    let msg0 = "ToEnumB"
    case toEnumMay $ fromIntegral a of
      Nothing -> do
         let msg1 = msg0 <> " out of range"
         pp <- eval (Proxy @def) opts (Proxy @(PP t a))
         pure $ case getValueLR opts msg1 pp [] of
           Left e -> e
           Right _ -> mkNode opts (_tBool pp) [msg1] [hh pp]
      Just n -> pure $ mkNode opts (PresentT n) [msg0 <> show0 opts " " n <> showA opts " | " a] []

-- | a predicate on prime numbers
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Prime 2
--   True
--   TrueT
--
--   >>> pl @(Map '(Id,Prime) Id) [0..12]
--   Present [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
--   PresentT [(0,False),(1,False),(2,True),(3,True),(4,False),(5,True),(6,False),(7,True),(8,False),(9,False),(10,False),(11,True),(12,False)]
--
data Prime

instance (Show a, Integral a) => P Prime a where
  type PP Prime a = Bool
  eval _ opts a =
    let b = isPrime $ fromIntegral a
    in pure $ mkNodeB opts b ["Prime" <> showA opts " | " a] []


-- | 'not' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Not False
--   True
--   TrueT
--
--   >>> pl @Not True
--   False
--   FalseT
--
data Not
instance a ~ Bool => P Not a where
  type PP Not a = Bool
  eval _ opts a =
    let b = not a
    in pure $ mkNodeB opts b ["Not"] [] -- already has FalseT TrueT so no need to add confusing data


-- empty lists at the type level wont work here

-- | filters a list \'q\' keeping or removing those elements in \'p\'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Keep '[5] '[1,5,5,2,5,2]) ()
--   Present [5,5,5]
--   PresentT [5,5,5]
--
--   >>> pl @(Keep '[0,1,1,5] '[1,5,5,2,5,2]) ()
--   Present [1,5,5,5]
--   PresentT [1,5,5,5]
--
--   >>> pl @(Remove '[5] '[1,5,5,2,5,2]) ()
--   Present [1,2,2]
--   PresentT [1,2,2]
--
--   >>> pl @(Remove '[0,1,1,5] '[1,5,5,2,5,2]) ()
--   Present [2,2]
--   PresentT [2,2]
--
--   >>> pl @(Remove '[99] '[1,5,5,2,5,2]) ()
--   Present [1,5,5,2,5,2]
--   PresentT [1,5,5,2,5,2]
--
--   >>> pl @(Remove '[99,91] '[1,5,5,2,5,2]) ()
--   Present [1,5,5,2,5,2]
--   PresentT [1,5,5,2,5,2]
--
--   >>> pl @(Remove Id '[1,5,5,2,5,2]) []
--   Present [1,5,5,2,5,2]
--   PresentT [1,5,5,2,5,2]
--
--   >>> pl @(Remove '[] '[1,5,5,2,5,2]) 44 -- works if you make this a number!
--   Present [1,5,5,2,5,2]
--   PresentT [1,5,5,2,5,2]
--
data KeepImpl (keep :: Bool) p q
type Remove p q = KeepImpl 'False p q
type Keep p q = KeepImpl 'True p q

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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let ret = filter (bool not id keep . (`elem` p)) q
        in mkNode opts (PresentT ret) [msg0 <> show0 opts " " ret <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | 'elem' function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Elem Fst Snd) ('x',"abcdxy")
--   True
--   TrueT
--
--   >>> pl @(Elem Fst Snd) ('z',"abcdxy")
--   False
--   FalseT
--
data Elem p q
type ElemAll p q = Any (Elem I q)

instance ([PP p a] ~ PP q a
         , P p a
         , P q a
         , Show (PP p a)
         , Eq (PP p a)
         ) => P (Elem p q) a where
  type PP (Elem p q) a = Bool
  eval _ opts a = do
    let msg0 = "Elem"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `elem` q
        in mkNodeB opts b [show p <> " `elem` " <> show q] [hh pp, hh qq]

-- | 'const' () function
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @() "Asf"
--   Present ()
--   PresentT ()
--
instance Show a => P () a where
  type PP () a = ()
  eval _ opts a = pure $ mkNode opts (PresentT ()) ["()" <> show0 opts " " a] []

type Head' p = HeadFail "Head(empty)" p
type Tail' p = TailFail "Tail(empty)" p
type Last' p = LastFail "Last(empty)" p
type Init' p = InitFail "Init(empty)" p

-- | similar to fmap fst
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Fmap_1 (Just (13,"Asf"))
--   Present Just 13
--   PresentT (Just 13)
--
-- to make this work we grab the fst or snd out of the Maybe so it is a head or not/ is a tail or not etc!
-- we still have access to the whole original list so we dont lose anything!
data Fmap_1
instance Functor f => P Fmap_1 (f (a,x)) where
  type PP Fmap_1 (f (a,x)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (fst <$> mb)) ["Fmap_1"] []

-- | similar to fmap snd
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Fmap_2 (Just ("asf",13))
--   Present Just 13
--   PresentT (Just 13)
--
data Fmap_2
instance Functor f => P Fmap_2 (f (x,a)) where
  type PP Fmap_2 (f (x,a)) = f a
  eval _ opts mb = pure $ mkNode opts (PresentT (snd <$> mb)) ["Fmap_2"] []

type HeadDef p q   = GDef (Uncons >> Fmap_1) p q
type HeadP q       = GProxy (Uncons >> Fmap_1) q
type HeadFail msg q = GFail (Uncons >> Fmap_1) msg q

type TailDef p q   = GDef (Uncons >> Fmap_2) p q
type TailP q       = GProxy (Uncons >> Fmap_2) q
type TailFail msg q = GFail (Uncons >> Fmap_2) msg q

type LastDef p q   = GDef (Unsnoc >> Fmap_2) p q
type LastP q       = GProxy (Unsnoc >> Fmap_2) q
type LastFail msg q = GFail (Unsnoc >> Fmap_2) msg q

type InitDef p q   = GDef (Unsnoc >> Fmap_1) p q
type InitP q       = GProxy (Unsnoc >> Fmap_1) q
type InitFail msg q = GFail (Unsnoc >> Fmap_1) msg q

-- 'x' and 'a' for Just condition
-- 'x' for Nothing condition
-- Snd at the end says we only want to process the Maybe which is the rhs of &&& ie Snd
type GDef' z p q r = '(I, r >> z) >> MaybeXP (X >> p) q Snd
type JustDef' p q r = GDef' I p q r

-- access everything ie 'x' and Proxy a for Nothing condition
-- 'x' and 'a' for Just condition
type GDef'' z p q r = '(I, r >> z) >> MaybeXP p q Snd
type JustDef'' p q r = GDef'' I p q r

type PA = Snd -- 'Proxy a' -- to distinguish from A
type A = Snd -- 'a'
type X = Fst >> Fst -- 'x' ie the whole original environment
type XA = I -- ie noop
type XPA = I -- ie noop

-- Nothing has access to 'x' only
-- Just has access to (x,a)
--type GDef_X z p q r = (I &&& (r >> z)) >> MaybeXP (Fst >> Fst >> p) ((Fst *** I) >> q) Snd
type GDef_X z p q r = '(I, r >> z) >> MaybeXP (X >> p) ('(X,A) >> q) A
type JustDef''' p q r = GDef_X I p q r

-- Nothing has access to 'Proxy a' only
-- Just has access to (x,a)
type GDef_PA z p q r = Hide % '(I, r >> z) >> MaybeXP (PA >> p) ('(X,A) >> q) A

-- Nothing case sees ((I,qz), Proxy a) -- hence the Fst >> Fst
-- Just case sees (I,qz), a) -- hence the Snd to get the 'a' only -- if you want the 'x' then Fst >> Fst
-- we have lost 'x' on the rhs: use GDef_X to access 'x' and 'a' for the Just condition
type GDef z p q     = '(I, q >> z) >> MaybeXP (X >> p) A A  -- Hide % immediately before MaybeXP
type GProxy z q     = '(I, q >> z) >> MaybeXP (PA >> MemptyP) A A
type GFail z msg q  = '(I, q >> z) >> MaybeXP (Fail (PA >> Unproxy) (X >> msg)) A A

-- use these!
type LookupDef' x y p q    = GDef (Lookup x y) p q
type LookupP' x y q        = GProxy (Lookup x y) q
type LookupFail' msg x y q = GFail (Lookup x y) msg q

type LookupDef x y p    = LookupDef' x y p I
type LookupP x y        = LookupP' x y I
type LookupFail msg x y = LookupFail' msg x y I

type Just' p    = JustFail  "expected Just" p
type Left' p    = LeftFail  "expected Left"  p
type Right' p   = RightFail "expected Right" p
type This' p    = ThisFail  "expected This"  p
type That'  p   = ThatFail  "expected That"  p
type TheseIn' p = TheseFail "expected These" p

type JustDef p q    = GDef I p q
type JustP q        = GProxy I q
type JustFail msg q = GFail I msg q

type LeftDef p q    = GDef LeftToMaybe p q
type LeftP q        = GProxy LeftToMaybe q
type LeftFail msg q = GFail LeftToMaybe msg q

type RightDef p q    = GDef RightToMaybe p q
type RightP q        = GProxy RightToMaybe q
type RightFail msg q = GFail RightToMaybe msg q

type ThisDef p q    = GDef ThisToMaybe p q
type ThisP q       = GProxy ThisToMaybe q
type ThisFail msg q = GFail ThisToMaybe msg q

type ThatDef p q    = GDef ThatToMaybe p q
type ThatP q       = GProxy ThatToMaybe q
type ThatFail msg q = GFail ThatToMaybe msg q

type TheseDef p q    = GDef TheseToMaybe p q
type TheseP q       = GProxy TheseToMaybe q
type TheseFail msg q = GFail TheseToMaybe msg q

-- tacks on a Proxy to Nothing side! but a Proxy a not Proxy of the final result
-- this is for default use cases for either/these/head/tail/last/init etc
data MaybeXP p q r
type MaybeX p q r = MaybeXP (Fst >> p) q r

instance (P r x
        , P p (x, Proxy a)
        , P q (x,a)
        , PP r x ~ Maybe a
        , PP p (x, Proxy a) ~ b
        , PP q (x,a) ~ b
        ) => P (MaybeXP p q r) x where
  type PP (MaybeXP p q r) x = MaybeXPT (PP r x) x q
  eval _ opts x = do
    let msg0 = "MaybeXP"
    rr <- eval (Proxy @r) opts x
    case getValueLR opts msg0 rr [] of
      Left e -> pure e
      Right Nothing -> do
        let msg1 = msg0 <> "(Nothing)"
        pp <- eval (Proxy @p) opts (x, Proxy @a)
        pure $ case getValueLR opts msg1 pp [hh rr] of
          Left e -> e
          Right _ -> mkNode opts (_tBool pp) [msg1] [hh rr, hh pp]
      Right (Just a) -> do
        let msg1 = msg0 <> "(Just)"
        qq <- eval (Proxy @q) opts (x,a)
        pure $ case getValueLR opts msg1 qq [hh rr] of
          Left e -> e
          Right _ -> mkNode opts (_tBool qq) [msg1] [hh rr, hh qq]

type family MaybeXPT lr x q where
  MaybeXPT (Maybe a) x q = PP q (x,a)


-- | similar to either Just (const Nothing)
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @LeftToMaybe (Left 13)
--   Present Just 13
--   PresentT (Just 13)
--
--   >>> pl @LeftToMaybe (Right 13)
--   Present Nothing
--   PresentT Nothing
--
data LeftToMaybe
instance P LeftToMaybe (Either a x) where
  type PP LeftToMaybe (Either a x) = Maybe a
  eval _ opts lr = pure $ mkNode opts (PresentT (either Just (const Nothing) lr)) ["LeftToMaybe"] []


-- | similar to either (const Nothing) Just
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @RightToMaybe (Right 13)
--   Present Just 13
--   PresentT (Just 13)
--
--   >>> pl @RightToMaybe (Left 13)
--   Present Nothing
--   PresentT Nothing
--
data RightToMaybe
instance P RightToMaybe (Either x a) where
  type PP RightToMaybe (Either x a) = Maybe a
  eval _ opts lr = pure $ mkNode opts (PresentT (either (const Nothing) Just lr)) ["RightToMaybe"] []

data ThisToMaybe

instance P ThisToMaybe (These a x) where
  type PP ThisToMaybe (These a x) = Maybe a
  eval _ opts th = pure $ mkNode opts (PresentT (these Just (const Nothing) (const . const Nothing) th)) ["ThisToMaybe"] []

data ThatToMaybe

instance P ThatToMaybe (These x a) where
  type PP ThatToMaybe (These x a) = Maybe a
  eval _ opts th = pure $ mkNode opts (PresentT (these (const Nothing) Just (const . const Nothing) th)) ["ThatToMaybe"] []

data TheseToMaybe

instance P TheseToMaybe (These a b) where
  type PP TheseToMaybe (These a b) = Maybe (a,b)
  eval _ opts th = pure $ mkNode opts (PresentT (these (const Nothing) (const Nothing) ((Just .) . (,)) th)) ["TheseToMaybe"] []

-- | similar to 'Control.Arrow.|||' but additionally gives \'p\' and \'q\' the original input
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(EitherX (((Fst >> Fst) + Snd) >> ShowP) ShowP Snd) (9,Left 123)
--   Present "132"
--   PresentT "132"
--
--   >>> pl @(EitherX (((Fst >> Fst) + Snd) >> ShowP) ShowP Snd) (9,Right 'x')
--   Present "((9,Right 'x'),'x')"
--   PresentT "((9,Right 'x'),'x')"
--
--   >>> pl @(EitherX ShowP (Second Succ >> ShowP) Snd) (9,Right 'x')
--   Present "((9,Right 'x'),'y')"
--   PresentT "((9,Right 'x'),'y')"
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

-- | similar to 'Data.These.mergeTheseWith' but additionally provides \'p\', '\q'\ and \'r\' the original input as the first element in the tuple
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(TheseX (((Fst >> Fst) + Snd) >> ShowP) ShowP (Snd >> Snd) Snd) (9,This 123)
--   Present "132"
--   PresentT "132"
--
--   >>> pl @(TheseX '(Snd,"fromthis") '(99 >> Negate,Snd) Snd Id) (This 123)
--   Present (123,"fromthis")
--   PresentT (123,"fromthis")
--
--   >>> pl @(TheseX '(Snd,"fromthis") '(99 >> Negate,Snd) Snd Id) (That "fromthat")
--   Present (-99,"fromthat")
--   PresentT (-99,"fromthat")
--
--   >>> pl @(TheseX '(Snd,"fromthis") '(99 >> Negate,Snd) Snd Id) (These 123 "fromthese")
--   Present (123,"fromthese")
--   PresentT (123,"fromthese")
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
--   similar to 'MaybeX' but provides a Proxy to the result of \'q\' and does not provide the surrounding context
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MaybeIn "foundnothing" (Pred >> ShowP)) (Just 20)
--   Present "19"
--   PresentT "19"
--
--   >>> pl @(MaybeIn "found nothing" (Pred >> ShowP)) Nothing
--   Present "found nothing"
--   PresentT "found nothing"
--
data MaybeIn p q
type IsNothing = MaybeIn 'True 'False
type IsJust = MaybeIn 'False 'True

-- tricky: the nothing case is the proxy of PP q a: ie proxy of the final result!!
-- this is different from MaybeXP which gives you a proxy of 'a' [you need both!]
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
          Right b -> mkNode opts (_tBool qq) [msg1 <> show0 opts " " b <> showA opts " | " a] [hh qq]


-- | similar to 'SG.stimes'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(STimes 4 Id) (SG.Sum 3)
--   Present Sum {getSum = 12}
--   PresentT (Sum {getSum = 12})
--
--   >>> pl @(STimes 4 Id) "ab"
--   Present "abababab"
--   PresentT "abababab"
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
    lr <- runPQ msg0 (Proxy @n) (Proxy @p) opts a
    pure $ case lr of
      Left e -> e
      Right (fromIntegral -> (n::Int),p,pp,qq) ->
        let msg1 = msg0 <> show0 opts " " n <> " p=" <> show p
            b = SG.stimes n p
            in mkNode opts (PresentT b) [msg1 <> show0 opts " " b <> showA opts " | n=" n <> showA opts " | " p] [hh pp, hh qq]


-- | similar to 'pure'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Pure Maybe Id) 4
--   Present Just 4
--   PresentT (Just 4)
--
--   >>> pl @(Pure [] Id) 4
--   Present [4]
--   PresentT [4]
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
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | " a] [hh pp]

type PMempty = MemptyT' 'Proxy  -- lifts 'a' to 'Proxy a' then we can use it with MemptyP

-- | similar to 'mempty'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MemptyT (SG.Sum Int)) ()
--   Present Sum {getSum = 0}
--   PresentT (Sum {getSum = 0})
--
-- no Monoid for Maybe a unless a is also a monoid but can use empty!
data MemptyT' t
type MemptyT (t :: Type) = MemptyT' (Hole t)
type MemptyP = MemptyT' Unproxy -- expects a proxy: so only some things work with this: eg Pad MaybeIn etc

instance (Show (PP t a), Monoid (PP t a)) => P (MemptyT' t) a where
  type PP (MemptyT' t) a = PP t a
  eval _ opts _ =
    let b = mempty @(PP t a)
    in pure $ mkNode opts (PresentT b) ["MemptyT" <> show0 opts " " b] []

data MemptyProxy
instance Monoid a => P MemptyProxy (Proxy (a :: Type)) where
  type PP MemptyProxy (Proxy a) = a
  eval _ opts _pa =
    let b = mempty @a
    in pure $ mkNode opts (PresentT b) ["MemptyProxy"] []

-- | similar to 'empty'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(EmptyT Maybe) ()
--   Present Nothing
--   PresentT Nothing
--
data EmptyT (t :: Type -> Type)
type MkNothing'' = EmptyT Maybe -- works if a->Maybe a: MkNothing will always work cos can override the output type

instance (Show (t a), Alternative t) => P (EmptyT t) a where
  type PP (EmptyT t) a = t a
  eval _ opts _ =
    let b = empty @t
    in pure $ mkNode opts (PresentT b) ["EmptyT" <> show0 opts " " b] []

data MkNothing' t -- works always! MaybeB is a good alternative and then dont need the extra 't'
type MkNothing (t :: Type) = MkNothing' (Hole t)

-- for this to be useful has to have 't' else we end up with tons of problems
instance P (MkNothing' t) a where
  type PP (MkNothing' t) a = Maybe (PP t a)
  eval _ opts _ =
    let msg = "MkNothing"
    in pure $ mkNode opts (PresentT Nothing) [msg] []

-- | 'Just' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @MkJust 44
--   Present Just 44
--   PresentT (Just 44)
--
data MkJust
instance Show a => P MkJust a where
  type PP MkJust a = Maybe a
  eval _ opts a =
    let msg0 = "MkJust"
        d = Just a
    in pure $ mkNode opts (PresentT d) [msg0 <> show0 opts " Just " a] []

-- | 'Data.Either.Left' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MkLeft _ Id) 44
--   Present Left 44
--   PresentT (Left 44)
--
data MkLeft' t p
type MkLeft (t :: Type) p = MkLeft' (Hole t) p

instance (Show (PP p x), P p x) => P (MkLeft' t p) x where
  type PP (MkLeft' t p) x = Either (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkLeft"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Left p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " Left " p] []

-- | 'Data.Either.Right' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MkRight _ Id) 44
--   Present Right 44
--   PresentT (Right 44)
--
data MkRight' t p
type MkRight (t :: Type) p = MkRight' (Hole t) p

instance (Show (PP p x), P p x) => P (MkRight' t p) x where
  type PP (MkRight' t p) x = Either (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkRight"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = Right p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " Right " p] []

-- | 'Data.These.This' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MkThis _ Id) 44
--   Present This 44
--   PresentT (This 44)
--
data MkThis' t p
type MkThis (t :: Type) p = MkThis' (Hole t) p

instance (Show (PP p x), P p x) => P (MkThis' t p) x where
  type PP (MkThis' t p) x = These (PP p x) (PP t x)
  eval _ opts x = do
    let msg0 = "MkThis"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = This p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " This " p] []

-- | 'Data.These.That' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MkThat _ Id) 44
--   Present That 44
--   PresentT (That 44)
--
data MkThat' t p
type MkThat (t :: Type) p = MkThat' (Hole t) p

instance (Show (PP p x), P p x) => P (MkThat' t p) x where
  type PP (MkThat' t p) x = These (PP t x) (PP p x)
  eval _ opts x = do
    let msg0 = "MkThat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = That p
        in mkNode opts (PresentT d) [msg0 <> show0 opts " That " p] []

--type MkThat t p = MkThis t p >> Swap
-- type MkThat' (t :: Type) = Pure (These t) Id -- t has to be a semigroup

-- | 'Data.These.These' constructor
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(MkThese Fst Snd) (44,'x')
--   Present These 44 'x'
--   PresentT (These 44 'x')
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = These p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d] [hh pp, hh qq]

-- | similar to 'mconcat'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @MConcat [SG.Sum 44, SG.Sum 12, SG.Sum 3]
--   Present Sum {getSum = 59}
--   PresentT (Sum {getSum = 59})
--
data MConcat
type FoldMap (t :: Type) p = Map (Wrap t Id) p >> MConcat >> Unwrap

type Sum (t :: Type) = FoldMap (SG.Sum t) Id
type Min' (t :: Type) = FoldMap (SG.Min t) Id -- requires t be Bounded for monoid instance

instance (Show a, Monoid a) => P MConcat [a] where
  type PP MConcat [a] = a
  eval _ opts a =
    let b = mconcat a
    in pure $ mkNode opts (PresentT b) ["MConcat" <> show0 opts " " b <> showA opts " | " a] []

-- | similar to 'concat'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Concat Id) ["abc","D","eF","","G"]
--   Present "abcDeFG"
--   PresentT "abcDeFG"
--
--   >>> pl @(Concat Snd) ('x',["abc","D","eF","","G"])
--   Present "abcDeFG"
--   PresentT "abcDeFG"
--
data Concat p

instance (Show a
        , Show (t [a])
        , PP p x ~ (t [a])
        , P p x
        , Foldable t
        ) => P (Concat p) x where
  type PP (Concat p) x = MapTX (PP p x)
  eval _ opts x = do
    let msg0 = "Concat"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right as ->
        let b = concat as
        in mkNode opts (PresentT b) ["Concat" <> show0 opts " " b <> showA opts " | " as] [hh pp]

instance P (Proxy t) a where
  type PP (Proxy t) a = Proxy t
  eval _ opts _ =
    pure $ mkNode opts (PresentT Proxy) ["Proxy"] []

data ProxyT' t
type ProxyT (t :: Type) = ProxyT' (Hole t)

instance Typeable t => P (ProxyT' (t :: Type)) a where
  type PP (ProxyT' t) a = Proxy (PP t a)
  eval _ opts _ =
    let t = showT @t
    in pure $ mkNode opts (PresentT Proxy) ["ProxyT(" <> show t ++ ")"] []

-- | similar to 'Data.List.!!'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Ix 4 "not found") ["abc","D","eF","","G"]
--   Present "G"
--   PresentT "G"
--
--   >>> pl @(Ix 40 "not found") ["abc","D","eF","","G"]
--   Present "not found"
--   PresentT "not found"
--
data Ix (n :: Nat) def
type Ix' (n :: Nat) = Ix n (Failp "Ix index not found")

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

-- | similar to 'Data.List.!!' leveraging 'Ixed'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> import qualified Data.Map.Strict as M
--   >>> pl @(Id !! 2) ["abc","D","eF","","G"]
--   Present "eF"
--   PresentT "eF"
--
--   >>> pl @(Id !! 20) ["abc","D","eF","","G"]
--   Error (!!) index not found
--   FailT "(!!) index not found"
--
--   >>> pl @(Id !! "eF") (M.fromList (flip zip [0..] ["abc","D","eF","","G"]))
--   Present 2
--   PresentT 2
--
data IxL p q def -- p is the big value and q is the index and def is the default
type p !! q = IxL p q (Failp "(!!) index not found")
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
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
             Just ret -> pure $ mkNode opts (PresentT ret) [msg1 <> show0 opts " " ret <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | 'lookup' leveraging 'Ixed'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> import qualified Data.Map.Strict as M
--   >>> pl @(Id !!! 2) ["abc","D","eF","","G"]
--   Present "eF"
--   PresentT "eF"
--
--   >>> pl @(Id !!! 20) ["abc","D","eF","","G"]
--   Error index not found
--   FailT "index not found"
--
--   >>> pl @(Id !!! "eF") (M.fromList (flip zip [0..] ["abc","D","eF","","G"]))
--   Present 2
--   PresentT 2
--
--   >>> pl @(Lookup Id 2) ["abc","D","eF","","G"]
--   Present Just "eF"
--   PresentT (Just "eF")
--
--   >>> pl @(Lookup Id 20) ["abc","D","eF","","G"]
--   Present Nothing
--   PresentT Nothing
--
data Lookup p q
type p !!! q = Lookup p q >> MaybeIn (Failp "index not found") Id -- use !!
-- Lookup' is interesting but just use Lookup or !!
type Lookup' (t :: Type) p q = q &&& Lookup p q >> If (Snd >> IsNothing) (Fst >> ShowP >> Fail (Hole t) (Printf "index(%s) not found" Id)) (Snd >> 'Just Id)


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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> "(" <> show q <> ")"
            hhs = [hh pp, hh qq]
        in case p ^? ix q of
             Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " not found"] hhs
             Just ret -> mkNode opts (PresentT (Just ret)) [msg1 <> show0 opts " " ret <> showA opts " | p=" p <> showA opts " | q=" q] hhs

-- | 'Data.List.ands'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Ands [True,True,True]
--   True
--   TrueT
--
--   >>> pl @Ands [True,True,True,False]
--   False
--   FalseT
--
--   >>> pl @Ands []
--   True
--   TrueT
--
data Ands
type Ands' = FoldMap SG.All Id

instance (as ~ t a
        , Show (t a)
        , Foldable t
        , a ~ Bool
        ) => P Ands as where
  type PP Ands as = Bool
  eval _ opts as =
    let b = and as
    in pure $ mkNodeB opts b ["Ands" <> showA opts " | " as] []

-- | 'Data.List.ors'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Ors [False,False,False]
--   False
--   FalseT
--
--   >>> pl @Ors [True,True,True,False]
--   True
--   TrueT
--
--   >>> pl @Ors []
--   False
--   FalseT
--
data Ors
type Ors' = FoldMap SG.Any Id

instance (as ~ t a
        , Show (t a)
        , Foldable t
        , a ~ Bool
        ) => P Ors as where
  type PP Ors as = Bool
  eval _ opts as =
    let b = or as
    in pure $ mkNodeB opts b ["Ors" <> showA opts " | " as] []

-- | similar to cons
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst :+ Snd) (99,[1,2,3,4])
--   Present [99,1,2,3,4]
--   PresentT [99,1,2,3,4]
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `cons` q
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | similar to snoc
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Snd +: Fst) (99,[1,2,3,4])
--   Present [1,2,3,4,99]
--   PresentT [1,2,3,4,99]
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = p `snoc` q
        in mkNode opts (PresentT b) [msg0 <> show0 opts " " b <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | 'Control.Lens.uncons'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Uncons [1,2,3,4]
--   Present Just (1,[2,3,4])
--   PresentT (Just (1,[2,3,4]))
--
--   >>> pl @Uncons []
--   Present Nothing
--   PresentT Nothing
--
data Uncons

instance (Show (ConsT s)
        , Show s
        , Cons s s (ConsT s) (ConsT s)
        ) => P Uncons s where
  type PP Uncons s = Maybe (ConsT s,s)
  eval _ opts as =
    let b = as ^? _Cons
    in pure $ mkNode opts (PresentT b) ["Uncons" <> show0 opts " " b <> showA opts " | " as] []

-- | 'Control.Lens.unsnoc'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Unsnoc [1,2,3,4]
--   Present Just ([1,2,3],4)
--   PresentT (Just ([1,2,3],4))
--
--   >>> pl @Unsnoc []
--   Present Nothing
--   PresentT Nothing
--
data Unsnoc

instance (Show (ConsT s)
        , Show s
        , Snoc s s (ConsT s) (ConsT s)
        ) => P Unsnoc s where
  type PP Unsnoc s = Maybe (s,ConsT s)
  eval _ opts as =
    let b = as ^? _Snoc
    in pure $ mkNode opts (PresentT b) ["Unsnoc" <> show0 opts " " b <> showA opts " | " as] []

-- | similar to 'null' using 'AsEmpty'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @IsEmpty [1,2,3,4]
--   False
--   FalseT
--
--   >>> pl @IsEmpty []
--   True
--   TrueT
--
--   >>> pl @IsEmpty LT
--   False
--   FalseT
--
--   >>> pl @IsEmpty EQ
--   True
--   TrueT
--
data IsEmpty

instance (Show as, AsEmpty as) => P IsEmpty as where
  type PP IsEmpty as = Bool
  eval _ opts as =
    let b = has _Empty as
    in pure $ mkNodeB opts b ["IsEmpty" <> showA opts " | " as] []

-- | similar to 'null' using 'Foldable'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Null [1,2,3,4]
--   False
--   FalseT
--
--   >>> pl @Null []
--   True
--   TrueT
--
data Null

instance (Show (t a)
        , Foldable t
        , t a ~ as
        ) => P Null as where
  type PP Null as = Bool
  eval _ opts as =
    let b = null as
    in pure $ mkNodeB opts b ["Null" <> showA opts " | " as] []

-- | similar to 'enumFromTo'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(EnumFromTo 2 5) ()
--   Present [2,3,4,5]
--   PresentT [2,3,4,5]
--
--   >>> pl @(EnumFromTo LT GT) ()
--   Present [LT,EQ,GT]
--   PresentT [LT,EQ,GT]
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts z
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) -> mkNode opts (PresentT (enumFromTo p q)) [msg0 <> " [" <> show p <> " .. " <> show q <> "]"] [hh pp, hh qq]

type MapMaybe p q = ConcatMap (p >> MaybeIn MemptyP '[Id]) q
type CatMaybes q = MapMaybe Id q

-- | similar to 'partitionEithers'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @PartitionEithers [Left 'a',Right 2,Left 'c',Right 4,Right 99]
--   Present ("ac",[2,4,99])
--   PresentT ("ac",[2,4,99])
--
data PartitionEithers

instance (Show a, Show b) => P PartitionEithers [Either a b] where
  type PP PartitionEithers [Either a b] = ([a], [b])
  eval _ opts as =
    let b = partitionEithers as
    in pure $ mkNode opts (PresentT b) ["PartitionEithers" <> show0 opts " " b <> showA opts " | " as] []

-- | similar to 'partitionThese'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @PartitionThese [This 'a', That 2, This 'c', These 'z' 1, That 4, These 'a' 2, That 99]
--   Present (("ac",[2,4,99]),[('z',1),('a',2)])
--   PresentT (("ac",[2,4,99]),[('z',1),('a',2)])
--
data PartitionThese
instance (Show a, Show b) => P PartitionThese [These a b] where
  type PP PartitionThese [These a b] = (([a], [b]), [(a, b)])
  eval _ opts as =
    let (x,y,z) = partitionThese as
        b = ((x,y),z)
    in pure $ mkNode opts (PresentT b) ["PartitionThese" <> show0 opts " " b <> showA opts " | " as] []

type Thiss = PartitionThese >> Fst >> Fst
type Thats = PartitionThese >> Fst >> Snd
type Theses = PartitionThese >> Snd

type CatMaybesa t = Foldl (Fst <> (Snd >> MaybeIn MemptyP '[Id])) (MemptyT t) Id
type CatMaybesx t = Foldl (JustDef' Fst ((Fst >> Fst >> Fst) +: Snd) Snd) (MemptyT [t]) Id
type CatMaybesy t = Foldl (JustDef'' (Fst >> Fst >> Fst) ((Fst >> Fst >> Fst) +: Snd) Snd) (MemptyT [t]) Id
type CatMaybesz t = Foldl (JustDef''' Fst ((Fst >> Fst) +: Snd) Snd) (MemptyT [t]) Id

-- want to pass Proxy b to q but then we have no way to calculate 'b'

-- | similar to 'scanl'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Scanl (Snd :+ Fst) Fst Snd) ([99],[1..5])
--   Present [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]
--   PresentT [[99],[1,99],[2,1,99],[3,2,1,99],[4,3,2,1,99],[5,4,3,2,1,99]]
--
data Scanl p q r
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
-- result is scanl but signature is flipped ((a,b) -> b) -> b -> [a] -> [b]

type ScanN n p q = Scanl (Fst >> q) p (EnumFromTo 1 n) -- n times using q then run p
type ScanNA q = ScanN Fst Snd q
type Repeat n p q = Last' (ScanN n p q)
type Foldl p q r = Last' (Scanl p q r)

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
    lr <- runPQ msg0 (Proxy @q) (Proxy @r) opts z
    case lr of
      Left e -> pure e
      Right (q,r,qq,rr) -> do
        let msg1 = msg0  -- <> show0 opts " " q <> show0 opts " " r
            ff i b as' rs
               | i >= _MX = pure (rs, Left $ mkNode opts (FailT (msg1 <> ":failed at i=" <> show i)) [msg1 <> " i=" <> show i <> " (b,as')=" <> show (b,as')] [])
               | otherwise =
                   case as' of
                     [] -> pure (rs, Right ()) -- ++ [((i,q), mkNode opts (PresentT q) [msg1 <> "(done)"] [])], Right ())
                     a:as -> do
                        pp :: TT b <- eval (Proxy @p) opts (b,a)
                        case getValueLR opts (msg1 <> " i=" <> show i <> " a=" <> show a) pp [] of
                           Left e  -> pure (rs,Left e)
                           Right b' -> ff (i+1) b' as (rs ++ [((i,b), pp)])
        (ts,lrx) :: ([((Int, b), TT b)], Either (TT [b]) ()) <- ff 1 q r []
        pure $ case splitAndAlign opts [msg1] (((0,q), mkNode opts (PresentT q) [msg1 <> "(initial)"] []) : ts) of
             Left _e -> error "cant happen!"
             Right (vals,itts) ->
               case lrx of
                 Left e -> mkNode opts (_tBool e) [msg1] (hh qq : hh rr : map (hh . fixit) itts ++ [hh e])
                 Right () -> mkNode opts (PresentT vals) [msg1 <> show0 opts " " vals <> showA opts " | b=" q <> showA opts " | as=" r] (hh qq : hh rr : map (hh . fixit) itts)

type family UnfoldT mbs where
  UnfoldT (Maybe (b,s)) = b

-- | similar to 'unfoldr'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Unfoldr (MaybeB (Null >> Not) (SplitAt 2 Id)) Id) [1..5]
--   Present [[1,2],[3,4],[5]]
--   PresentT [[1,2],[3,4],[5]]
--
data Unfoldr p q
--type Iteraten (t :: Type) n f = Unfoldr (If (Fst == 0) (MkNothing t) (Snd &&& (Pred *** f) >> MkJust)) '(n, Id)
type IterateN n f = Unfoldr (MaybeB (Fst > 0) '(Snd, Pred *** f)) '(n, Id)
type IterateUntil p f = IterateWhile (p >> Not) f
type IterateWhile p f = Unfoldr (MaybeB p '(Id, f)) Id
type IterateNWhile n p f = '(n, Id) >> IterateWhile (Fst > 0 && (Snd >> p)) (Pred *** f) >> Map Snd Id
type IterateNUntil n p f = IterateNWhile n (p >> Not) f

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
            ff i s rs | i >= _MX = pure (rs, Left $ mkNode opts (FailT (msg1 <> ":failed at i=" <> show i)) [msg1 <> " i=" <> show i <> " s=" <> show s] [])
                      | otherwise = do
                              pp :: TT (PP p s) <- eval (Proxy @p) opts s
                              case getValueLR opts (msg1 <> " i=" <> show i <> " s=" <> show s) pp [] of
                                   Left e  -> pure (rs, Left e)
                                   Right Nothing -> pure (rs, Right ())
                                   Right w@(Just (_b,s')) -> ff (i+1) s' (rs ++ [((i,w), pp)])
        (ts,lr) :: ([((Int, PP p s), TT (PP p s))], Either (TT [b]) ()) <- ff 1 q []
        pure $ case splitAndAlign opts [msg1] ts of
             Left _e -> error "cant happen"
             Right (vals, itts) ->
               case lr of
                 Left e -> mkNode opts (_tBool e) [msg1] (hh qq : map (hh . fixit) itts ++ [hh e])
                 Right () ->
                   let ret = fst <$> catMaybes vals
                   in mkNode opts (PresentT ret) [msg1 <> show0 opts " " ret <> showA opts " | s=" q ] (hh qq : map (hh . fixit) itts)

-- | similar to 'map'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Map Pred Id) [1..5]
--   Present [0,1,2,3,4]
--   PresentT [0,1,2,3,4]
--
data Map p q
type ConcatMap p q = Concat (Map p q)

instance (Show (PP p a)
        , P p a
        , PP q x ~ f a
        , P q x
        , Show a
        , Show (f a)
        , Foldable f
        ) => P (Map p q) x where
  type PP (Map p q) x = [PP p (MapTX (PP q x))]
  eval _ opts x = do
    let msg0 = "Map"
    qq <- eval (Proxy @q) opts x
    case getValueLR opts msg0 qq [] of
      Left e -> pure e
      Right as -> do
        ts <- zipWithM (\i a -> ((i, a),) <$> eval (Proxy @p) opts a) [0::Int ..] (toList as)
        pure $ case splitAndAlign opts [msg0] ts of
             Left e -> e
             Right (vals, _) -> mkNode opts (PresentT vals) [msg0 <> show0 opts " " vals <> showA opts " | " as] (hh qq : map (hh . fixit) ts)

type family MapTX ta where
  MapTX (t a) = a

-- | if p then run q else run r
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(If (Gt 4) "greater than 4" "less than or equal to 4" ) 10
--   Present "greater than 4"
--   PresentT "greater than 4"
--
--   >>> pl @(If (Gt 4) "greater than 4" "less than or equal to 4") 0
--   Present "less than or equal to 4"
--   PresentT "less than or equal to 4"
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

-- | creates a list of overlapping pairs of elements
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Pairs [1,2,3,4]
--   Present [(1,2),(2,3),(3,4)]
--   PresentT [(1,2),(2,3),(3,4)]
--
--   >>> pl @Pairs []
--   Error Pairs no data found
--   FailT "Pairs no data found"
--
--   >>> pl @Pairs [1]
--   Error Pairs only one element found
--   FailT "Pairs only one element found"
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
         Right zs -> mkNode opts (PresentT zs) [msg0 <> show0 opts " " zs <> showA opts " | " as ] []


-- | similar to 'partition'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Partition (Ge 3) Id) [10,4,1,7,3,1,3,5]
--   Present ([10,4,7,3,3,5],[1,1])
--   PresentT ([10,4,7,3,3,5],[1,1])
--
data Partition p q
type FilterBy p q = Partition p q >> Fst

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
      Right as -> do
             ts <- zipWithM (\i a -> ((i, a),) <$> evalBool (Proxy @p) opts a) [0::Int ..] as
             pure $ case splitAndAlign opts [msg0] ts of
               Left e -> e
               Right (vals, tfs) ->
                 let w0 = partition fst $ zip vals tfs
                     zz1 = (map (snd . fst . snd) *** map (snd . fst . snd)) w0
                 in mkNode opts (PresentT zz1) [msg0 <> show0 opts " " zz1 <> showA opts " | s=" as] (hh qq : map (hh . fixit) tfs)


-- | similar to 'break'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Break (Ge 3) Id) [10,4,1,7,3,1,3,5]
--   Present ([],[10,4,1,7,3,1,3,5])
--   PresentT ([],[10,4,1,7,3,1,3,5])
--
--   >>> pl @(Break (Lt 3) Id) [10,4,1,7,3,1,3,5]
--   Present ([10,4],[1,7,3,1,3,5])
--   PresentT ([10,4],[1,7,3,1,3,5])
--
data Break p q
type Span p q = Break (p >> Not) q
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
      Right as -> do
        let ff [] zs = pure (zs, [], Nothing) -- [(ia,qq)] extras | the rest of the data | optional last pivot or error
            ff ((i,a):ias) zs = do
               pp <- evalBool (Proxy @p) opts a
               let v = ((i,a), pp)
               case getValueLR opts msg0 pp [hh qq] of
                 Right False -> ff ias (zs :> v)
                 Right True -> pure (zs,map snd ias,Just v)
                 Left _ -> pure (zs,map snd ias,Just v)
        (ialls,rhs,mpivot) <- ff (zip [0::Int ..] as) Seq.empty
        pure $ case mpivot of
             Nothing ->
               mkNode opts (PresentT (map (snd . fst) (toList ialls), rhs))
                       ([msg0] <> ["cnt=" <> show (length ialls, length rhs)])
                       (map (hh . fixit) (toList ialls))
             Just iall@(ia, tt) ->
               case getValueLR opts (msg0 <> " predicate failed") tt (hh qq : map (hh . fixit) (toList (ialls :> iall))) of
                 Right True ->
                   mkNode opts (PresentT (map (snd . fst) (toList ialls), snd ia : rhs))
                           ([msg0] <> ["cnt=" <> show (length ialls, 1+length rhs)])
                           (hh qq : hh tt : map (hh . fixit) (toList (ialls :> iall)))

                 Right False -> error "shouldnt happen"
                 Left e -> e

-- | Fails the computation with a message
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Failt Int (Printf "value=%03d" Id)) 99
--   Error value=099
--   FailT "value=099"
--
--   >>> pl @(FailS (Printf2 "value=%03d string=%s")) (99,"somedata")
--   Error value=099 string=somedata
--   FailT "value=099 string=somedata"
--
data Fail t prt -- t=output type prt=msg
type Failp s = Fail Unproxy s
type Failt (t :: Type) prt = Fail (Hole t) prt
type FailS s = Fail I s
type FailPrt (t :: Type) prt = Fail (Hole t)(Printf prt)
type FailPrt2 (t :: Type) prt = Fail (Hole t)(Printf2 prt)

instance (P prt a
        , PP prt a ~ String
        ) => P (Fail t prt) a where
  type PP (Fail t prt) a = PP t a
  eval _ opts a = do
    let msg = "Fail"
    pp <- eval (Proxy @prt) opts a
    pure $ case getValueLR opts msg pp [] of
      Left e -> e
      Right s -> mkNode opts (FailT s) [msg <> " " <> s] [hh pp]

data Hole (t :: Type)
type T (t :: Type) = Hole t -- easier to type

-- | Acts as a proxy in this dsl where you can explicitly set the Type.
--
--  It is passed around as an argument to help the type checker when needed.
--  see 'ReadP', 'ParseTimeP', 'ShowP'
--
instance Typeable t => P (Hole t) a where
  type PP (Hole t) a = t -- can only be Type not Type -> Type (can use Proxy but then we go down the rabbithole)
  eval _ opts _a =
    let msg = "Hole(" <> showT @t <> ")"
    in pure $ mkNode opts (FailT msg) [msg <> " you probably meant to get access to the type of PP only and not evaluate"] []

data Unproxy

instance Typeable a => P Unproxy (Proxy (a :: Type)) where
  type PP Unproxy (Proxy a) = a
  eval _ opts _a =
    let msg = "Unproxy(" <> showT @a <> ")"
    in pure $ mkNode opts (FailT msg) [msg <> " you probably meant to get access to the type of PP only and not evaluate"] []

-- | transparent predicate wrapper to make k of kind Type so it can be in a promoted list (cant mix kinds) see 'Do'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Do '[W 123, W "xyz", Len &&& Id, Pred *** Id<>Id]) ()
--   Present (2,"xyzxyz")
--   PresentT (2,"xyzxyz")
--
--
--   >>> pl @(TupleI '[W 999,W "somestring",W 'True, Id, Pred >> ShowP]) 23
--   Present (999,("somestring",(True,(23,("22",())))))
--   PresentT (999,("somestring",(True,(23,("22",())))))
--
data W (p :: k)
instance P p a => P (W p) a where
  type PP (W p) a = PP p a
  eval _ = eval (Proxy @(Msg "W" p))

-- | catch a failure
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Catch Succ (Fst >> Second ShowP >> Printf2 "%s %s" >> 'LT)) GT
--   Present LT
--   PresentT LT
--
--   >>> pl @(Catch' Succ (Second ShowP >> Printf2 "%s %s")) GT
--   Error Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT
--   FailT "Succ IO e=Prelude.Enum.Ordering.succ: bad argument GT"
--
--   >>> pl @(Catch' Succ (Second ShowP >> Printf2 "%s %s")) LT
--   Present EQ
--   PresentT EQ
--
-- more flexible: takes a (String,x) and a proxy so we can still call 'False 'True
-- now takes the FailT string and x so you can print more detail if you want
-- need the proxy so we can fail without having to explicitly specify a type
data Catch p q -- catch p and if fails runs q only on failt
type Catch' p s = Catch p (FailCatch s) -- eg set eg s=Printf "%d" Id or Printf "%s" ShowP
type FailCatch s = Fail (Snd >> Unproxy) (Fst >> s)

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

type Even = Mod I 2 >> Same 0
type Odd = Mod I 2 >> Same 1
type Div' p q = DivMod p q >> Fst
type Mod' p q = DivMod p q >> Snd

-- | similar to 'div'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Div Fst Snd) (10,4)
--   Present 2
--   PresentT 2
--
--   >>> pl @(Div Fst Snd) (10,0)
--   Error Div zero denominator
--   FailT "Div zero denominator"
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
    let msg = "Div"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg <> " zero denominator")) [msg <> " zero denominator"] hhs
              _ -> let d = p `div` q
                   in mkNode opts (PresentT d) [show p <> " `div` " <> show q <> " = " <> show d] hhs


-- | similar to 'mod'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Mod Fst Snd) (10,3)
--   Present 1
--   PresentT 1
--
--   >>> pl @(Mod Fst Snd) (10,0)
--   Error Mod zero denominator
--   FailT "Mod zero denominator"
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
    let msg = "Mod"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
         let hhs = [hh pp, hh qq]
         in case q of
              0 -> mkNode opts (FailT (msg <> " zero denominator")) [msg <> " zero denominator"] hhs
              _ -> let d = p `mod` q
                   in mkNode opts (PresentT d) [show p <> " `mod` " <> show q <> " = " <> show d] hhs

-- | similar to 'divMod'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(DivMod Fst Snd) (10,3)
--   Present (3,1)
--   PresentT (3,1)
--
--   >>> pl @(DivMod Fst Snd) (10,-3)
--   Present (-4,-2)
--   PresentT (-4,-2)
--
--   >>> pl @(DivMod Fst Snd) (-10,3)
--   Present (-4,2)
--   PresentT (-4,2)
--
--   >>> pl @(DivMod Fst Snd) (-10,-3)
--   Present (3,-1)
--   PresentT (3,-1)
--
--   >>> pl @(DivMod Fst Snd) (10,0)
--   Error DivMod zero denominator
--   FailT "DivMod zero denominator"
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
    let msg = "DivMod"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg <> " zero denominator")) [msg <> " zero denominator"] hhs
             _ -> let d = p `divMod` q
                  in mkNode opts (PresentT d) [show p <> " `divMod` " <> show q <> " = " <> show d] hhs

-- | similar to 'quotRem'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(QuotRem Fst Snd) (10,3)
--   Present (3,1)
--   PresentT (3,1)
--
--   >>> pl @(QuotRem Fst Snd) (10,-3)
--   Present (-3,1)
--   PresentT (-3,1)
--
--   >>> pl @(QuotRem Fst Snd) (-10,-3)
--   Present (3,-1)
--   PresentT (3,-1)
--
--   >>> pl @(QuotRem Fst Snd) (-10,3)
--   Present (-3,-1)
--   PresentT (-3,-1)
--
--   >>> pl @(QuotRem Fst Snd) (10,0)
--   Error QuotRem zero denominator
--   FailT "QuotRem zero denominator"
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
    let msg = "QuotRem"
    lr <- runPQ msg (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
        in case q of
             0 -> mkNode opts (FailT (msg <> " zero denominator")) [msg <> " zero denominator"] hhs
             _ -> let d = p `quotRem` q
                  in mkNode opts (PresentT d) [show p <> " `quotRem` " <> show q <> " = " <> show d] hhs

type Quot p q = QuotRem p q >> Fst
type Rem p q = QuotRem p q >> Snd

--type OneP = Guard "expected list of length 1" (Len >> Same 1) >> Head'
type OneP = Guard (Printf "expected list of length 1 but found length=%d" Len) (Len >> Same 1) >> Head

strictmsg :: forall strict . GetBool strict => String
strictmsg = if getBool @strict then "" else "Lax"

-- k or prt has access to (Int,a) where Int is the current guard position: hence need to use Printf2
-- todo: better explanation of how this works
-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out)

-- | Guards contain a type level list of tuples the action to run on failure of the predicate and the predicate itself
--   Each tuple validating against the corresponding value in a value list
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 4)]) [17,4]
--   Present [17,4]
--   PresentT [17,4]
--
--   >>> pl @(Guards '[ '("arg1 failed",Gt 4), '("arg2 failed", Same 5)]) [17,4]
--   Error arg2 failed
--   FailT "arg2 failed"
--
--   >>> pl @(Guards '[ '("arg1 failed",Gt 99), '("arg2 failed", Same 4)]) [17,4]
--   Error arg1 failed
--   FailT "arg1 failed"
--
--   >>> pl @(Guards '[ '(Printf2 "arg %d failed with value %d",Gt 4), '(Printf2 "%d %d", Same 4)]) [17,3]
--   Error 1 3
--   FailT "1 3"
--
--   >>> pl @(GuardsQuick (Printf2 "arg %d failed with value %d") '[Gt 4, Ge 3, Same 4]) [17,3,5]
--   Error arg 2 failed with value 5
--   FailT "arg 2 failed with value 5"
--
--   >>> pl @(GuardsQuick (Printf2 "arg %d failed with value %d") '[Gt 4, Ge 3, Same 4]) [17,3,5,99]
--   Error Guards: data elements(4) /= predicates(3)
--   FailT "Guards: data elements(4) /= predicates(3)"
--
data GuardsImpl (n :: Nat) (strict :: Bool) (os :: [(k,k1)])
type Guards (os :: [(k,k1)]) = GuardsImplW 'True os
type GuardsLax (os :: [(k,k1)]) = GuardsImplW 'False os
type GuardsQuick (prt :: k) (os :: [k1]) = Guards (ToGuardsT prt os)

data GuardsImplW (strict :: Bool) (ps :: [(k,k1)])
instance (GetBool strict, GetLen ps, P (GuardsImpl (LenT ps) strict ps) [a]) => P (GuardsImplW strict ps) [a] where
  type PP (GuardsImplW strict ps) [a] = PP (GuardsImpl (LenT ps) strict ps) [a]
  eval _ opts as = do
    let strict = getBool @strict
        msgbase0 = "Guards" <> strictmsg @strict
        n = getLen @ps
    if strict && n /= length as then
       let xx = msgbase0 <> ": data elements(" <> show (length as) <> ") /= predicates(" <> show n <> ")"
       in pure $ mkNode opts (FailT xx) [xx] []
    else eval (Proxy @(GuardsImpl (LenT ps) strict ps)) opts as

instance (KnownNat n
        , GetBool strict
        , Show a
        ) => P (GuardsImpl n strict ('[] :: [(k,k1)])) [a] where
  type PP (GuardsImpl n strict ('[] :: [(k,k1)])) [a] = [a]
  eval _ opts as =
    let msg = "Guards" <> strictmsg @strict <> "(" <> show n <> ")"
        n :: Int = nat @n
    in pure $ mkNode opts (PresentT as) [msg <> " done!" <> if null as then "" else showA opts " | leftovers=" as] []

instance (PP prt (Int, a) ~ String
        , P prt (Int, a)
        , KnownNat n
        , GetBool strict
        , GetLen ps
        , P p a
        , PP p a ~ Bool
        , P (GuardsImpl n strict ps) [a]
        , PP (GuardsImpl n strict ps) [a] ~ [a]
        , Show a
        ) => P (GuardsImpl n strict ('(prt,p) ': ps)) [a] where
  type PP (GuardsImpl n strict ('(prt,p) ': ps)) [a] = [a]
  eval _ opts as' = do
     let msgbase0 = "Guards" <> strictmsg @strict <> "(" <> show (n-pos) <> ":" <> show n <> ")"
         msgbase1 = "Guard" <> strictmsg @strict <> "(" <> show (n-pos) <> ")"
         msgbase2 = "Guards" <> strictmsg @strict
         n :: Int = nat @n
         pos = getLen @ps
     case as' of
         [] -> pure $ mkNode opts mempty [msgbase0 <> " (ran out of data!!)"] []
         a:as -> do
                    pp <- evalBool (Proxy @p) opts a
                    case getValueLR opts (msgbase1 <> " p failed") pp [] of
                         Left e -> pure e
                         Right False -> do
                           qq <- eval (Proxy @prt) opts (n-pos-1,a) -- only run prt when predicate is False
                           pure $ case getValueLR opts (msgbase2 <> " False predicate and prt failed") qq [hh pp] of
                              Left e -> e
                              Right msgx -> mkNode opts (FailT msgx) [msgbase1 <> " failed [" <> msgx <> "]" <> show0 opts " " a] [hh pp, hh qq]
                         Right True -> do
                           ss <- eval (Proxy @(GuardsImpl n strict ps)) opts as
                           pure $ case getValueLRHide opts (msgbase1 <> " ok | rhs failed") ss [hh pp] of
                             Left e -> e -- shortcut else we get too compounding errors with the pp tree being added each time!
                             Right zs -> mkNode opts (PresentT (a:zs)) [msgbase1 <> show0 opts " " a] [hh pp, hh ss]

-- | \'p\' is the predicate and on failure of the predicate runs \'prt\'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Guard "expected > 3" (Gt 3)) 17
--   Present 17
--   PresentT 17
--
--   >>> pl @(Guard "expected > 3" (Gt 3)) 1
--   Error expected > 3
--   FailT "expected > 3"
--
--   >>> pl @(Guard (Printf "%d not > 3" Id) (Gt 3)) (-99)
--   Error -99 not > 3
--   FailT "-99 not > 3"
--
data Guard prt p
type Guard' p = Guard "Guard" p

type ExitWhen prt p = Guard prt (p >> Not)
type ExitWhen' p = ExitWhen "ExitWhen" p

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
    case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> pure e
      Right False -> do
        qq <- eval (Proxy @prt) opts a
        pure $ case getValueLR opts (msg0 <> " Msg") qq [hh pp] of
          Left e -> e
          Right msgx -> mkNode opts (FailT msgx) [msg0 <> "(failed) [" <> msgx <> "]" <> show0 opts " | " a] [hh pp, hh qq]
      Right True -> pure $ mkNode opts (PresentT a) [msg0 <> "(ok)" <> show0 opts " | " a] [hh pp]  -- dont show the guard message if successful

-- just run the effect but skip the value: eg use for Stdout so doesnt interfere with 'a' unless there is an error
data Skip p
type p |> q = Skip p >> q
infixr 1 |>
type p >| q = p >> Skip q
infixr 1 >|

instance (Show (PP p a), P p a) => P (Skip p) a where
  type PP (Skip p) a = a
  eval _ opts a = do
    let msg0 = "Skip"
    pp <- eval (Proxy @p) opts a
    pure $ case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> e
      Right p -> mkNode opts (PresentT a) [msg0 <> show0 opts " " p] [hh pp]

-- advantage of (>>) over 'Do [k] is we can use different kinds for (>>) without having to wrap in W

-- | This is composition for predicates
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst >> Id !! 0 >> Succ) ([11,12],'x')
--   Present 12
--   PresentT 12
--
--   >>> pl @(Len *** Succ >> First Pred >> ShowP) ([11,12],'x')
--   Present "(1,'y')"
--   PresentT "(1,'y')"
--

data (p :: k) >> (q :: k1)
infixr 1 >>

type (<<) p q = q >> p
infixl 1 <<

instance (Show (PP p a)
        , Show (PP q (PP p a))
        , P p a
        , P q (PP p a)
        ) => P (p >> q) a where
  type PP (p >> q) a = PP q (PP p a)
  eval _ opts a = do
    let msg = ">>"
    pp <- eval (Proxy @p) opts a
    case getValueLRHide opts "lhs failed >>" pp [] of
      Left e -> pure e
      Right p -> do
        qq <- eval (Proxy @q) opts p
        pure $ case getValueLRHide opts (show p <> " >> rhs failed") qq [hh pp] of
          Left e -> e
          Right q -> mkNode opts (_tBool qq) [msg <> show0 opts " " q <> showA opts " | " p] [hh pp, hh qq]

-- | similar to 'Prelude.&&'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst && (Snd >> Len >> Ge 4)) (True,[11,12,13,14])
--   True
--   TrueT
--
--   >>> pl @(Fst && (Snd >> Len >> Same 4)) (True,[12,11,12,13,14])
--   False
--   FalseT
--
data (&&) (p :: k) (q :: k1)
type And p q = p && q
infixr 3 &&

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p && q) a where
  type PP (p && q) a = Bool
  eval _ opts a = do
    pp <- evalBool (Proxy @p) opts a
    qq <- evalBool (Proxy @q) opts a
    pure $ evalBinStrict opts "&&" (&&) pp qq

-- | similar to 'Prelude.||'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst || (Snd >> Len >> Ge 4)) (False,[11,12,13,14])
--   True
--   TrueT
--
--   >>> pl @((Fst >> Not) || (Snd >> Len >> Same 4)) (True,[12,11,12,13,14])
--   False
--   FalseT
--
data (||) (p :: k) (q :: k1)
type OR p q = p || q
infixr 2 ||

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p || q) a where
  type PP (p || q) a = Bool
  eval _ opts a = do
    pp <- evalBool (Proxy @p) opts a
    qq <- evalBool (Proxy @q) opts a
    pure $ evalBinStrict opts "||" (||) pp qq

-- | implication
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst ~> (Snd >> Len >> Ge 4)) (True,[11,12,13,14])
--   True
--   TrueT
--
--   >>> pl @(Fst ~> (Snd >> Len >> Same 4)) (True,[12,11,12,13,14])
--   False
--   FalseT
--
--   >>> pl @(Fst ~> (Snd >> Len >> Same 4)) (False,[12,11,12,13,14])
--   True
--   TrueT
--
--   >>> pl @(Fst ~> (Snd >> Len >> Ge 4)) (False,[11,12,13,14])
--   True
--   TrueT
--
data (~>) (p :: k) (q :: k1)
type Imply p q = p ~> q
infixr 1 ~>

instance (P p a
        , P q a
        , PP p a ~ Bool
        , PP q a ~ Bool
        ) => P (p ~> q) a where
  type PP (p ~> q) a = Bool
  eval _ opts a = do
    pp <- evalBool (Proxy @p) opts a
    qq <- evalBool (Proxy @q) opts a
    pure $ evalBinStrict opts "~>" imply pp qq

data OrdP p q
type p === q = OrdP p q
infix 4 ===

-- | similar to 'compare'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(OrdP Fst Snd) (10,9)
--   Present GT
--   PresentT GT
--
--   >>> pl @(OrdP Fst Snd) (10,10)
--   Present EQ
--   PresentT EQ
--
--   >>> pl @(OrdP Fst Snd) (10,11)
--   Present LT
--   PresentT LT
--
type OrdA' p q = OrdP (Fst >> p) (Snd >> q)
type OrdA p = OrdA' p p

instance (Ord (PP p a)
        , PP p a ~ PP q a
        , P p a
        , Show (PP q a)
        , P q a
        ) => P (OrdP p q) a where
  type PP (OrdP p q) a = Ordering
  eval _ opts a = do
    let msg0 = "OrdP"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = compare p q
        in mkNode opts (PresentT d) [msg0 <> " " <> show p <> " " <> prettyOrd d <> show0 opts " " q] [hh pp, hh qq]

-- | compare two strings ignoring case
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst ===? Snd) ("abC","aBc")
--   Present EQ
--   PresentT EQ
--
--   >>> pl @(Fst ===? Snd) ("abC","DaBc")
--   Present LT
--   PresentT LT
--
data OrdI p q
type p ===? q = OrdI p q
infix 4 ===?

instance (PP p a ~ String
        , PP p a ~ PP q a
        , P p a
        , P q a
        ) => P (OrdI p q) a where
  type PP (OrdI p q) a = Ordering
  eval _ opts a = do
    let msg0 = "OrdI"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = on compare (map toLower) p q
        in mkNode opts (PresentT d) [msg0 <> " " <> p <> " " <> prettyOrd d <> " " <> q] [hh pp, hh qq]

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
    lr <- runPQ sfn (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = fn p q
        in mkNodeB opts b [show p <> " " <> sfn <> show0 opts " " q] [hh pp, hh qq]

-- for strings
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
    lr <- runPQ sfn (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = on fn (map toLower) p q
        in mkNodeB opts b ["CmpI " <> p <> " " <> sfn <> " " <> q] [hh pp, hh qq]

type Gt n = Cmp 'Cgt I n
type Ge n = Cmp 'Cge I n
type Same n = Cmp 'Ceq I n
type Le n = Cmp 'Cle I n
type Lt n = Cmp 'Clt I n
type Ne n = Cmp 'Cne I n

-- | similar to 'Control.Lens.itoList'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(IToList _) ("aBc" :: String)
--   Present [(0,'a'),(1,'B'),(2,'c')]
--   PresentT [(0,'a'),(1,'B'),(2,'c')]
--
data IToList' t p
type IToList (t :: Type) = IToList' (Hole t) Id

instance (Show x
        , P p x
        , Typeable (PP t (PP p x))
        , Show (PP t (PP p x))
        , FoldableWithIndex (PP t (PP p x)) f
        , PP p x ~ f a
        , Show a
        ) => P (IToList' t p) x where
  type PP (IToList' t p) x = [(PP t (PP p x), UnIToListT (PP p x))]
  eval _ opts x = do
    let msg0 = "IToList"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> e
      Right p ->
        let b = itoList p
            t = showT @(PP t (PP p x))
        in mkNode opts (PresentT b) [msg0 <> "(" <> t <> ")" <> show0 opts " " b <> showA opts " | " x] [hh pp]

type family UnIToListT fa where
  UnIToListT (f a) = a

-- | similar to 'toList'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @ToList "aBc"
--   Present "aBc"
--   PresentT "aBc"
--
data ToList
instance (Show (t a)
        , Foldable t
        , Show a
        ) => P ToList (t a) where
  type PP ToList (t a) = [a]
  eval _ opts as =
    let z = toList as
    in pure $ mkNode opts (PresentT z) ["ToList" <> show0 opts " " z <> showA opts " | " as] []

data ToListExt

instance (Show l
        , Ge.IsList l
        , Show (Ge.Item l)
        ) => P ToListExt l where
  type PP ToListExt l = [Ge.Item l]
  eval _ opts as =
    let z = Ge.toList as
    in pure $ mkNode opts (PresentT z) ["ToListExt" <> show0 opts " " z <> showA opts " | " as] []

data FromList (t :: Type) -- doesnt work with OverloadedLists unless you cast to [a] explicitly

instance (a ~ Ge.Item t
        , Show t
        , Ge.IsList t
        ) => P (FromList t) [a] where
  type PP (FromList t) [a] = t
  eval _ opts as =
    let z = Ge.fromList (as :: [Ge.Item t]) :: t
    in pure $ mkNode opts (PresentT z) ["FromList" <> show0 opts " " z] []

data FromListF (t :: Type) -- works only with overloadedlists
-- l ~ l' is key
instance (Show l
        , Ge.IsList l
        , l ~ l'
        ) => P (FromListF l') l where
  type PP (FromListF l') l = l'
  eval _ opts as =
     let z = Ge.fromList (Ge.toList @l as)
     in pure $ mkNode opts (PresentT z) ["FromListF" <> show0 opts " " z] []

-- | predicate on 'These'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @IsThis (This "aBc")
--   True
--   TrueT
--
--   >>> pl @IsThis (These 1 'a')
--   False
--   FalseT
--
--   >>> pl @IsThese (These 1 'a')
--   True
--   TrueT
--
data IsTh (th :: These x y) -- x y can be anything

type IsThis = IsTh ('This '())
type IsThat = IsTh ('That '())
type IsThese = IsTh ('These '() '())

-- trying to avoid show instance cos of ambiguities
instance (Show a
        , Show b
        , GetThese th
        ) => P (IsTh (th :: These x y)) (These a b) where
  type PP (IsTh th) (These a b) = Bool
  eval _ opts th =
     let (t,f) = getThese (Proxy @th)
         b = f th
     in pure $ mkNodeB opts b ["IsTh '" <> t <> showA opts " | " th] []

-- | similar to 'these'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(TheseIn Id Len (Fst + Length Snd)) (This 13)
--   Present 13
--   PresentT 13
--
--   >>> pl @(TheseIn Id Len (Fst + Length Snd)) (That "this is a long string")
--   Present 21
--   PresentT 21
--
--   >>> pl @(TheseIn Id Len (Fst + Length Snd)) (These 20 "somedata")
--   Present 28
--   PresentT 28
--
--   >>> pl @(TheseIn (Left _) (Right _) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (That "this is a long string")
--   Present Right "this is a long string"
--   PresentT (Right "this is a long string")
--
--   >>> pl @(TheseIn (Left _) (Right _) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 1 "this is a long string")
--   Present Right "this is a long string"
--   PresentT (Right "this is a long string")
--
--   >>> pl @(TheseIn (Left _) (Right _) (If (Fst > Length Snd) (MkLeft _ Fst) (MkRight _ Snd))) (These 100 "this is a long string")
--   Present Left 100
--   PresentT (Left 100)
--
data TheseIn p q r
type Theseid p q = TheseIn '(I, p) '(q, I) I

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
          let msg = "This"
          pp <- eval (Proxy @p) opts a
          pure $ case getValueLR opts (msg <> " p failed") pp [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [msg <> show0 opts " " c <> showA opts " | This " a] [hh pp]
        That b -> do
          let msg = "That"
          qq <- eval (Proxy @q) opts b
          pure $ case getValueLR opts (msg <> " q failed") qq [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [msg <> show0 opts " " c <> showA opts " | That " b] [hh qq]
        These a b -> do
          let msg = "TheseIn"
          rr <- eval (Proxy @r) opts (a,b)
          pure $ case getValueLR opts (msg <> " r failed") rr [] of
               Left e -> e
               Right c -> mkNode opts (PresentT c) [msg <> show0 opts " " c <> showA opts " | " (These a b)] [hh rr]

-- | extracts the first character from a non empty 'Symbol'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Char1 "aBc") ()
--   Present 'a'
--   PresentT 'a'
--
data Char1 (s :: Symbol)  -- gets the first char from the Symbol [requires that Symbol is not empty]
instance (KnownSymbol s, NullT s ~ 'False) => P (Char1 s) a where
  type PP (Char1 s) a = Char
  eval _ opts _ =
     let c = head $ symb @s
     in pure $ mkNode opts (PresentT c) ["Char1" <> show0 opts " " c] []

-- | similar to 'Data.Align.align' thats pads with 'Data.These.This' or 'Data.These.That' if one list is shorter than the other
--
--   the key is that all information about both lists are preserved
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ZipThese Fst Snd) ("aBc", [1..5])
--   Present [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
--   PresentT [These 'a' 1,These 'B' 2,These 'c' 3,That 4,That 5]
--
--   >>> pl @(ZipThese Fst Snd) ("aBcDeF", [1..3])
--   Present [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
--   PresentT [These 'a' 1,These 'B' 2,These 'c' 3,This 'D',This 'e',This 'F']
--
data ZipThese p q

instance (PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (ZipThese p q) a where
  type PP (ZipThese p q) a = [These (ArrT (PP p a)) (ArrT (PP q a))]
  eval _ opts a = do
    let msg0 = "ZipThese"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = TA.align p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

data ZipTheseF p q

instance (Show (f y)
        , PP p a ~ f x
        , PP q a ~ f y
        , ExtractT (f x) ~ x
        , ExtractT (f y) ~ y
        , Show (f x)
        , TA.Align f
        , Show (f (These x y))
        , P p a
        , P q a)
  => P (ZipTheseF p q) a where
  type PP (ZipTheseF p q) a = ApplyConstT (PP p a) (These (ExtractT (PP p a)) (ExtractT (PP q a)))
  eval _ opts a = do
    let msg0 = "ZipTheseF"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = TA.align p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

type family ExtractT (ta :: Type) :: Type where
  ExtractT (t a) = a
  ExtractT ta = GL.TypeError (
      'GL.Text "ExtractT: expected (t a) but found something else"
      ':$$: 'GL.Text "t a = "
      ':<>: 'GL.ShowType ta)

-- todo: get ArrT error to fire if wrong Type

-- | Zip two lists optionally cycling the one of the lists to match the size
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Ziplc Fst Snd) ("abc", [1..5])
--   Present [('a',1),('b',2),('c',3),('a',4),('b',5)]
--   PresentT [('a',1),('b',2),('c',3),('a',4),('b',5)]
--
--   >>> pl @(Ziplc Fst Snd) ("abcdefg", [1..5])
--   Present [('a',1),('b',2),('c',3),('d',4),('e',5)]
--   PresentT [('a',1),('b',2),('c',3),('d',4),('e',5)]
--
--   >>> pl @(Ziprc Fst Snd) ("abcdefg", [1..5])
--   Present [('a',1),('b',2),('c',3),('d',4),('e',5),('f',1),('g',2)]
--   PresentT [('a',1),('b',2),('c',3),('d',4),('e',5),('f',1),('g',2)]
--
data Zip (lc :: Bool) (rc :: Bool) p q
type Ziplc p q = Zip 'True 'False p q
type Ziprc p q = Zip 'False 'True p q
type Zipn p q = Zip 'False 'False p q

instance (GetBool lc
        , GetBool rc
        , PP p a ~ [x]
        , PP q a ~ [y]
        , P p a
        , P q a
        , Show x
        , Show y
        ) => P (Zip lc rc p q) a where
  type PP (Zip lc rc p q) a = [(ArrT (PP p a), ArrT (PP q a))]
  eval _ opts a = do
    let msg0 = "Zip" <> cyc
        lc = getBool @lc
        rc = getBool @rc
        cyc = case (lc,rc) of
               (True,False) -> "LC"
               (False,True) -> "RC"
               _ -> ""
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = case (lc,rc) of
                  (True,False) -> zip (take (length q) (cycle p)) q
                  (False,True) -> zip p (take (length p) (cycle q))
                  _ -> zip p q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | Luhn predicate check on last digit
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Luhn [1,2,3,0]
--   True
--   TrueT
--
--   >>> pl @Luhn [1,2,3,4]
--   False
--   FalseT
data Luhn


instance a ~ [Int] => P Luhn a where
  type PP Luhn a = Bool
  eval _ opts as =
    let xs = zipWith (*) (reverse as) (cycle [1,2])
        ys = map (\x -> if x>=10 then x-9 else x) xs
        z = sum ys
        ret = z `mod` 10
        msg = "Luhn"
    in pure $ if ret == 0 then mkNode opts TrueT [msg <> show0 opts " | " as] []
       else mkNode opts FalseT [msg <> " map=" <> show ys <> " sum=" <> show z <> " ret=" <> show ret <> showA opts " | as=" as] []

pe0, pe, pe1, pe2, pex, pe3, pl, plc :: forall p a . (Show (PP p a), P p a) => a -> IO (BoolT (PP p a))
pe0  = peWith @p o0
pe  = peWith @p o02
pex  = peWith @p o03
pe1 = peWith @p o1
pe2 = peWith @p o2
pe3 = peWith @p o3
pl = peWith @p ol
plc = peWith @p olc

peWith :: forall p a . (Show (PP p a), P p a) =>  -- Typeable (Proxy p),
     POpts -> a -> IO (BoolT (PP p a))
peWith opts a = do
  pp <- eval (Proxy @p) opts a
  let r = pp ^. tBool
  if oLite opts then
    let f = colorMe opts (r ^. boolT2P)
    in putStrLn $ case r of
         FailT e -> f "Error" <> " " <> e
         TrueT -> f "True"
         FalseT -> f "False"
         PresentT x -> f "Present" <> " " <> show x
  else prtTree opts (fromTT pp)
  return r

-- could get n::Nat as a predicate but it is fine as is!
-- | Read a number base 2 via 36
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ReadBase Int 16) "00feD"
--   Present 4077
--   PresentT 4077
--
--   >>> pl @(ReadBase Int 16) "-ff"
--   Present -255
--   PresentT (-255)
--
--   >>> pl @(ReadBase Int 2) "10010011"
--   Present 147
--   PresentT 147
--
-- supports negative numbers unlike readInt
data ReadBase' t (n :: Nat) p
type ReadBase (t :: Type) (n :: Nat) = ReadBase' (Hole t) n Id
type ReadBaseInt (n :: Nat) = ReadBase' (Hole Int) n Id


instance (Typeable (PP t x)
        , BetweenT 2 36 n
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
    pure $ case getValueLR opts (msg0 <> " p failed") pp [] of
      Left e -> e
      Right p ->
        let (ff,p1) = case p of
                        '-':q -> (negate,q)
                        _ -> (id,p)
        in case readInt (fromIntegral n)
            ((`elem` xs) . toLower)
            (fromJust . (`elemIndex` xs) . toLower)
            p1 of
             [(b,"")] -> mkNode opts (PresentT (ff b)) [msg0 <> show0 opts " " (ff b) <> showA opts " | " p] [hh pp]
             o -> mkNode opts (FailT ("invalid base " <> show n)) [msg0 <> " as=" <> p <> " err=" <> show o] [hh pp]

getValidBase :: Int -> String
getValidBase n = take n (['0'..'9'] <> ['a'..'z'])

-- | Display a number at base 2 to 36
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ShowBase 16) 4077
--   Present "fed"
--   PresentT "fed"
--
--   >>> pl @(ShowBase 16) (-255)
--   Present "-ff"
--   PresentT "-ff"
--
--   >>> pl @(ShowBase 2) 147
--   Present "10010011"
--   PresentT "10010011"
--
-- supports negative numbers unlike showIntAtBase
data ShowBase (n :: Nat)

instance (Show a
        , 2 GL.<= n
        , n GL.<= 36
        , KnownNat n
        , Integral a
        ) => P (ShowBase n) a where
  type PP (ShowBase n) a = String
  eval _ opts a =
    let n = nat @n
        xs = getValidBase n
        msg = "ShowBase " <> show n
        (ff,a') = if a < 0 then (('-':), abs a) else (id,a)
        b = showIntAtBase (fromIntegral n) (xs !!) a' ""
    in pure $ mkNode opts (PresentT (ff b)) [msg <> showLit0 opts " " (ff b) <> showA opts " | " a] []

type Assocl = '(I *** Fst, Snd >> Snd)
type Assocr = '(Fst >> Fst, Snd *** I)
--type Assocl = (I *** Fst) &&& (Snd >> Snd)
--type Assocr = (Fst >> Fst) &&& (Snd *** I)

-- | Intercalate
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Intercalate '["aB"] '["xxxx","yz","z","www","xyz"]) ()
--   Present ["xxxx","aB","yz","aB","z","aB","www","aB","xyz"]
--   PresentT ["xxxx","aB","yz","aB","z","aB","www","aB","xyz"]
--
data Intercalate p q

instance (PP p a ~ [x]
        , PP q a ~ PP p a
        , P p a
        , P q a
        , Show x
      ) => P (Intercalate p q) a where
  type PP (Intercalate p q) a = PP p a
  eval _ opts a = do
    let msg0 = "Intercalate"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = intercalate p (map (:[]) q)
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | " p <> showA opts " | " q] [hh pp, hh qq]

getStringPrefix :: String -> (String,String)
getStringPrefix = fix (\k z -> \case
                                   [] -> (z,[])
                                   '%':x:xs | x == '%' -> k (z <> ['%']) xs
                                            | otherwise -> (z,'%':x:xs)
                                   x:xs -> k (z <> [x]) xs
                      ) []

-- | uses Printf to format output
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Printf "value=%03d" Id) 12
--   Present "value=012"
--   PresentT "value=012"
--
-- splits string into pieces before "%" that way we have a chance of catching any errors
data Printf s p

instance (PrintfArg (PP p x)
        , Show (PP p x)
        , PP s x ~ String
        , P s x
        , P p x
        ) => P (Printf s p) x where
  type PP (Printf s p) x = String
  eval _ opts x = do
    let msg0 = "Printf"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x
    case lrx of
      Left e -> pure e
      Right (s,p,ss,pp) -> do
        let msg1 = msg0
        lr <- catchitNF @_ @E.SomeException (printf s p)
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg1 <> " (" <> e <> ")")) [msg1 <> show0 opts " " p <> " s=" <> s] [hh ss, hh pp]
          Right ret -> mkNode opts (PresentT ret) [msg1 <> " [" <> showLit0 opts "" ret <> "]" <> showA opts " | p=" p <> showLit opts " | s=" s] [hh ss, hh pp]

type family GuardsT (ps :: [k]) where
  GuardsT '[] = '[]
  GuardsT (p ': ps) = Guard' p ': GuardsT ps

type Guards' (ps :: [k]) = Para (GuardsT ps)

type ToPara (os :: [k]) = Proxy (ParaImplW 'True os)

type ToGuards (prt :: k) (os :: [k1]) = Proxy (Guards (ToGuardsT prt os))

type family ToGuardsT (prt :: k) (os :: [k1]) :: [(k,k1)] where
--  ToGuardsT prt '[] = '[]  -- error condition
  ToGuardsT prt '[p] = '(prt,p) : '[]
  ToGuardsT prt (p ': ps) = '(prt,p) ': ToGuardsT prt ps

-- | runs values in parallel unlike 'Do'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Para '[Id,Id + 1,Id * 4]) [10,20,30]
--   Present [10,21,120]
--   PresentT [10,21,120]
--
data ParaImpl (n :: Nat) (strict :: Bool) (os :: [k])
type Para (os :: [k]) = ParaImplW 'True os
type ParaLax (os :: [k]) = ParaImplW 'False os

data ParaImplW (strict :: Bool) (ps :: [k])

type family GuardsViaParaT prt ps where
  GuardsViaParaT prt '[] = '[]
  GuardsViaParaT prt (p ': ps) = Guard prt p ': GuardsViaParaT prt ps

type GuardsViaPara prt ps = Para (GuardsViaParaT prt ps)

-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance (GetBool strict, GetLen ps, P (ParaImpl (LenT ps) strict ps) [a]) => P (ParaImplW strict ps) [a] where
  type PP (ParaImplW strict ps) [a] = PP (ParaImpl (LenT ps) strict ps) [a]
  eval _ opts as = do
    let strict = getBool @strict
        msgbase0 = "Para" <> strictmsg @strict
        n = getLen @ps
    if strict && n /= length as then
       let xx = msgbase0 <> ": data elements(" <> show (length as) <> ") /= predicates(" <> show n <> ")"
       in pure $ mkNode opts (FailT xx) [xx] []
    else eval (Proxy @(ParaImpl (LenT ps) strict ps)) opts as

-- only allow non empty lists!
instance GL.TypeError ('GL.Text "ParaImpl '[] invalid: requires at least one value in the list")
   => P (ParaImpl n strict ('[] :: [k])) [a] where
  type PP (ParaImpl n strict ('[] :: [k])) [a] = Void
  eval _ _ _ = error "should not get this far"

-- forall k (p :: k) (n :: Nat) (strict :: Bool) a .
instance (Show (PP p a)
        , KnownNat n
        , GetBool strict
        , Show a
        , P p a
        ) => P (ParaImpl n strict '[p]) [a] where
  type PP (ParaImpl n strict '[p]) [a] = [PP p a]
  eval _ opts as' = do
    let strict = getBool @strict
        msgbase0 = "Para" <> strictmsg @strict
        msgbase1 = msgbase0 <> "(" <> show n <> ")"
        n :: Int
        n = nat @n
    case as' of
      [] -> pure $ mkNode opts mempty [msgbase1 <> " (ran out of data!!)"] []
      a:as -> do
        pp <- eval (Proxy @p) opts a
        pure $ case getValueLR opts msgbase1 pp [] of
          Left e -> e
          -- showA opts " " [b]  fails but using 'b' is ok and (b : []) also works!
          -- Ge.List error
          Right b -> mkNode opts (PresentT [b]) [msgbase1 <> (if null as then " done!" else " Truncated") <> show0 opts " " (b : []) <> showA opts " | " a <> (if strict then "" else showA opts " | leftovers=" as)] [hh pp]

instance (KnownNat n
        , GetBool strict
        , GetLen ps
        , P p a
        , P (ParaImpl n strict (p1 ': ps)) [a]
        , PP (ParaImpl n strict (p1 ': ps)) [a] ~ [PP p a]
        , Show a
        , Show (PP p a)
        )
     => P (ParaImpl n strict (p ': p1 ': ps)) [a] where
  type PP (ParaImpl n strict (p ': p1 ': ps)) [a] = [PP p a]
  eval _ opts as' = do
     let msgbase0 = msgbase2 <> "(" <> show (n-pos) <> " of " <> show n <> ")"
         msgbase1 = msgbase2 <> "(" <> show (n-pos) <> ")"
         msgbase2 = "Para" <> strictmsg @strict
         n = nat @n
         pos = 1 + getLen @ps -- cos p1!
     case as' of
       [] -> pure $ mkNode opts mempty [msgbase0 <> " (ran out of data!!)"] []
       a:as -> do
         pp <- eval (Proxy @p) opts a
         case getValueLR opts msgbase0 pp [] of
           Left e -> pure e
           Right b -> do
                        qq <- eval (Proxy @(ParaImpl n strict (p1 ': ps))) opts as
                        pure $ case getValueLRHide opts (msgbase1 <> " rhs failed " <> show b) qq [hh pp] of
                          Left e -> e
                          Right bs -> mkNode opts (PresentT (b:bs)) [msgbase1 <> show0 opts " " (b:bs) <> showA opts " | " as'] [hh pp, hh qq]

-- | tries each predicate ps and on the first match runs the corresponding qs but if there is no match on ps then runs the fail case e
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Case (FailS "asdf" >> Snd >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[Printf "%d is lt4" Id, Printf "%d is lt10" Id, Printf "%d is same50" Id] Id) 50
--   Present "50 is same50"
--   PresentT "50 is same50"
--
--   >>> pl @(Case (FailS "asdf" >> Snd >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[Printf "%d is lt4" Id, Printf "%d is lt10" Id, Printf "%d is same50" Id] Id) 9
--   Present "9 is lt10"
--   PresentT "9 is lt10"
--
--   >>> pl @(Case (FailS "asdf" >> Snd >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[Printf "%d is lt4" Id, Printf "%d is lt10" Id, Printf "%d is same50" Id] Id) 3
--   Present "3 is lt4"
--   PresentT "3 is lt4"
--
--   >>> pl @(Case (FailS "asdf" >> Snd >> Unproxy ) '[Lt 4,Lt 10,Same 50] '[Printf "%d is lt4" Id, Printf "%d is lt10" Id, Printf "%d is same50" Id] Id) 99
--   Error asdf
--   FailT "asdf"
--
data CaseImpl (n :: Nat) (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)
-- ps = conditions
-- qs = what to do [one to one
-- r = the value
-- e = otherwise  -- leave til later
data Case (e :: k0) (ps :: [k]) (qs :: [k1]) (r :: k2)
type Case' (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (Snd >> Failp "Case:no match") ps qs r
type Case'' s (ps :: [k]) (qs :: [k1]) (r :: k2) = Case (FailCase s) ps qs r -- eg s= Printf "%s" ShowP

type FailCase p = Fail (Snd >> Unproxy) (Fst >> p)


-- passthru but adds the length of ps (replaces LenT in the type synonym to avoid type synonyms being expanded out
instance (FailIfT (NotT (LenT ps DE.== LenT qs))
                  ('GL.Text "lengths are not the same "
                   ':<>: 'GL.ShowType (LenT ps)
                   ':<>: 'GL.Text " vs "
                   ':<>: 'GL.ShowType (LenT qs))
        , P (CaseImpl (LenT ps) e ps qs r) x
        ) => P (Case e ps qs r) x where
  type PP (Case e ps qs r) x = PP (CaseImpl (LenT ps) e ps qs r) x
  eval _ = eval (Proxy @(CaseImpl (LenT ps) e ps qs r))

-- only allow non empty lists!
instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lhs requires at least one value in the list"))
   => P (CaseImpl n e ('[] :: [k]) (q ': qs) r) x where
  type PP (CaseImpl n e ('[] :: [k]) (q ': qs) r) x = Void
  eval _ _ _ = error "should not get this far"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: rhs requires at least one value in the list"))
   => P (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e (p ': ps) ('[] :: [k1]) r) x = Void
  eval _ _ _ = error "should not get this far"

instance (GL.TypeError ('GL.Text "CaseImpl '[] invalid: lists are both empty"))
   => P (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x where
  type PP (CaseImpl n e ('[] :: [k]) ('[] :: [k1]) r) x = Void
  eval _ _ _ = error "should not get this far"

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
              Right b -> mkNode opts (PresentT b) [msgbase0 <> show0 opts " " b <> showA opts " | " a] [hh rr, hh pp, hh qq]
          Right False -> do
            ee <- eval (Proxy @e) opts (a, Proxy @(PP q (PP r x)))
            pure $ case getValueLR opts (msgbase0 <> "  otherwise failed") ee [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [msgbase0 <> show0 opts " " b <> showA opts " | " a] [hh rr, hh pp, hh ee]

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
    let msgbase0 = msgbase2 <> "(" <> show (n-pos) <> " of " <> show n <> ")"
        msgbase1 = msgbase2 <> "(" <> show (n-pos) <> ")"
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
            pure $ case getValueLR opts msgbase0 qq [hh rr] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [msgbase0 <> show0 opts " " b <> showA opts " | " a] [hh rr, hh pp, hh qq]
          Right False -> do
            ww <- eval (Proxy @(CaseImpl n e (p1 ': ps) (q1 ': qs) r)) opts z
            pure $ case getValueLR opts (msgbase1 <> " failed rhs") ww [hh rr, hh pp] of
              Left e -> e
              Right b -> mkNode opts (PresentT b) [msgbase1 <> show0 opts " " b <> showA opts " | " a] [hh rr, hh pp, hh ww]

-- | similar to 'sequenceA'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Sequence [Just 10, Just 20, Just 30]
--   Present Just [10,20,30]
--   PresentT (Just [10,20,30])
--
--   >>> pl @Sequence [Just 10, Just 20, Just 30, Nothing, Just 40]
--   Present Nothing
--   PresentT Nothing
--
data Sequence
type Traverse p q = Map p q >> Sequence


instance (Show (f (t a))
        , Show (t (f a))
        , Traversable t
        , Applicative f
        ) => P Sequence (t (f a)) where
  type PP Sequence (t (f a)) = f (t a)
  eval _ opts tfa =
     let d = sequenceA tfa
     in pure $ mkNode opts (PresentT d) ["Sequence" <> show0 opts " " d <> showA opts " | " tfa] []

data Hide p
type H = Hide
-- type H p = Hide p -- doesnt work with %   -- unsaturated!

instance P p a => P (Hide p) a where
  type PP (Hide p) a = PP p a
  eval _ opts a = do
      tt <- eval (Proxy @(Msg "!" p)) opts a
      pure $ tt & tForest .~ []

-- | similar to 'readFile'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(ReadFile ".ghci" >> 'Just Id >> Len >> Gt 0) ()
--   True
--   TrueT
--
--   >>> pl @(FileExists "xyzzy") ()
--   False
--   FalseT
--
data ReadFile p
type FileExists p = ReadFile p >> IsJust

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
          Nothing -> mkNode opts (FailT (msg1 <> " must run in IO")) [msg1 <> " must run in IO"] []
          Just Nothing -> mkNode opts (PresentT Nothing) [msg1 <> " does not exist"] []
          Just (Just b) -> mkNode opts (PresentT (Just b)) [msg1 <> " len=" <> show (length b) <> showLit0 opts " Just " b] []

-- | does the directory exists
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(DirExists ".") ()
--   True
--   TrueT
--
data ReadDir p
type DirExists p = ReadDir p >> IsJust

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
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(DirExists ".") ()
--   True
--   TrueT
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

data TimeU

instance P TimeU a where
  type PP TimeU a = UTCTime
  eval _ opts _a = do
    let msg = "TimeU"
    mb <- runIO $ getCurrentTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg <> " must run in IO")) [msg <> " must run in IO"] []
      Just v -> mkNode opts (PresentT v) [msg <> show0 opts " " v] []

data TimeZ

instance P TimeZ a where
  type PP TimeZ a = ZonedTime
  eval _ opts _a = do
    let msg = "TimeZ"
    mb <- runIO $ getZonedTime
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg <> " must run in IO")) [msg <> " must run in IO"] []
      Just v -> mkNode opts (PresentT v) [msg <> show0 opts " " v] []

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

data WritefileImpl (hh :: FHandle Symbol) p
type Appendfile (s :: Symbol) p = WritefileImpl ('FOther s 'WFAppend) p
type Writefile' (s :: Symbol) p = WritefileImpl ('FOther s 'WFWriteForce) p
type Writefile (s :: Symbol) p = WritefileImpl ('FOther s 'WFWrite) p
type Stdout p = WritefileImpl 'FStdout p
type Stderr p = WritefileImpl 'FStderr p

instance (GetFHandle fh
        , P p a
        , PP p a ~ String
        ) => P (WritefileImpl fh p) a where
  type PP (WritefileImpl fh p) a = ()
  eval _ opts a = do
    let fh = getFHandle @fh
        msg = case fh of
                      FStdout -> "Stdout"
                      FStderr -> "Stderr"
                      FOther s w -> (<>("[" <> s <> "]")) $ case w of
                         WFAppend -> "Appendfile"
                         WFWrite -> "Writefile"
                         WFWriteForce -> "Writefile'"
    pp <- eval (Proxy @p) opts a
    case getValueLR opts msg pp [] of
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
            Nothing -> mkNode opts (FailT (msg <> " must run in IO")) [msg <> " must run in IO"] [hh pp]
            Just (Left e) -> mkNode opts (FailT e) [msg <> " " <> e] [hh pp]
            Just (Right ()) -> mkNode opts (PresentT ()) [msg] [hh pp]

data Stdin

instance P Stdin a where
  type PP Stdin a = String
  eval _ opts _a = do
    let msg = "Stdin"
    mb <- runIO $ do
                      lr <- E.try $ hGetContents stdin
                      pure $ case lr of
                        Left (e :: E.SomeException) -> Left $ show e
                        Right ss -> Right ss
    pure $ case mb of
      Nothing -> mkNode opts (FailT (msg <> " must run in IO")) [msg <> " must run in IO"] []
      Just (Left e) -> mkNode opts (FailT e) [msg <> " " <> e] []
      Just (Right ss) -> mkNode opts (PresentT ss) [msg <> "[" <> showLit opts "" ss <> "]"] []

--type Just' = JustFail "expected Just" Id
type Nothing' = Guard "expected Nothing" IsNothing

-- | 'isInfixOf' 'isPrefixOf' 'isSuffixOf' equivalents
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(IsInfixI "abc" "axAbCd") ()
--   True
--   TrueT
--
--   >>> pl @(IsPrefixI "abc" "aBcbCd") ()
--   True
--   TrueT
--
--   >>> pl @(IsPrefix "abc" "aBcbCd") ()
--   False
--   FalseT
--
-- prefix infix suffix for strings
data IsFixImpl (cmp :: Ordering) (ignore :: Bool) p q

type IsPrefix p q = IsFixImpl 'LT 'False p q
type IsInfix p q = IsFixImpl 'EQ 'False p q
type IsSuffix p q = IsFixImpl 'GT 'False p q

type IsPrefixI p q = IsFixImpl 'LT 'True p q
type IsInfixI p q = IsFixImpl 'EQ 'True p q
type IsSuffixI p q = IsFixImpl 'GT 'True p q

instance (GetBool ignore
        , P p a
        , P q a
        , PP p a ~ String
        , PP q a ~ String
        , GetOrdering cmp
        ) => P (IsFixImpl cmp ignore p q) a where
  type PP (IsFixImpl cmp ignore p q) a = Bool
  eval _ opts a = do
    let cmp = getOrdering @cmp
        ignore = getBool @ignore
        lwr = if ignore then map toLower else id
        (ff,msg0) = case cmp of
                    LT -> (isPrefixOf, "IsPrefix")
                    EQ -> (isInfixOf, "IsInfix")
                    GT -> (isSuffixOf, "IsSuffix")
    pp <- eval (Proxy @p) opts a
    case getValueLR opts (msg0 <> " p failed") pp [] of
        Left e -> pure e
        Right s0 -> do
          let msg1 = msg0 <> (if ignore then "I" else "") <> "(" <> s0 <> ")"
          qq <- eval (Proxy @q) opts a
          pure $ case getValueLR opts (msg1 <> " q failed") qq [hh pp] of
            Left e -> e
            Right s1 -> mkNodeB opts (on ff lwr s0 s1) [msg1 <> showLit0 opts " " s1] [hh pp, hh qq]

-- | similar to 'SG.<>'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst <> Snd) ("abc","def")
--   Present "abcdef"
--   PresentT "abcdef"
--
data p <> q
infixr 6 <>
type Sapa' (t :: Type) = Wrap t Fst <> Wrap t Snd
type Sapa = Fst <> Snd

instance (Semigroup (PP p a)
        , PP p a ~ PP q a
        , P p a
        , Show (PP q a)
        ,P q a
        ) => P (p <> q) a where
  type PP (p <> q) a = PP p a
  eval _ opts a = do
    let msg0 = "<>"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <> q
        in mkNode opts (PresentT d) [show p <> " <> " <> show q <> " = " <> show d] [hh pp, hh qq]

runPQ :: (P p a, P q a, MonadEval m)
   => String
   -> Proxy p
   -> Proxy q
   -> POpts
   -> a
   -> m (Either (TT x) (PP p a, PP q a, TT (PP p a), TT (PP q a)))
runPQ msg0 proxyp proxyq opts a = do
    pp <- eval proxyp opts a
    case getValueLR opts msg0 pp [] of
      Left e -> pure $ Left e
      Right p -> do
         qq <- eval proxyq opts a
         pure $ case getValueLR opts msg0 qq [hh pp] of
           Left e -> Left e
           Right q -> Right (p, q, pp, qq)


-- have to reverse the inductive tuples cos cant figure out how to reverse generically
-- uses inductive tuples to replace variable args
class PrintC x where
  prtC :: (PrintfArg a, PrintfType r) => String -> (a,x) -> r
instance PrintC () where
  prtC s (a,()) = printf s a
instance (PrintfArg a, PrintC rs) => PrintC (a,rs) where
  prtC s (a,rs) = prtC s rs a

data TupleListImpl (strict :: Bool) (n :: Nat)
type TupleList (n :: Nat) = TupleListImpl 'True n
type TupleListLax (n :: Nat) = TupleListImpl 'False n

instance (Show a
        , KnownNat n
        , GetBool strict
        , TupleListD (ToN n) a
        , Show (TupleListT (ToN n) a)
        ) => P (TupleListImpl strict n) [a] where
  type PP (TupleListImpl strict n) [a] = TupleListT (ToN n) a
  eval _ opts as = do
    let strict = getBool @strict
        n :: Int = nat @n
        msg = "TupleList" <> (if strict then "" else "Lax") <> "(" <> show n <> ")"
    pure $ case tupleListD @(ToN n) @a strict as of
      Left e -> mkNode opts (FailT (msg <> " " <> e)) [msg <> " " <> e] []
      Right ret -> mkNode opts (PresentT ret) [msg <> show0 opts " " ret <> showA opts " | " as] []

data ReverseTupleN

instance (ReverseTupleC tp
        , Show (ReverseTupleP tp)
        , Show tp
        ) => P ReverseTupleN tp where
  type PP ReverseTupleN tp = ReverseTupleP tp
  eval _ opts tp =
    let ret = reverseTupleC tp
    in pure $ mkNode opts (PresentT ret) ["ReverseTupleN" <> show0 opts " " ret <> showA opts " | " tp] []

-- | Printfn prints
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Printfn "%s %s" Id) ("123",("def",()))
--   Present "123 def"
--   PresentT "123 def"
--
--   >>> pl @(Printfn "s=%s d=%03d" Id) ("ab",(123,()))
--   Present "s=ab d=123"
--   PresentT "s=ab d=123"
--
data Printfn s p
type Printfnt (n :: Nat) s =  Printfn s (TupleList n)
type PrintfntLax (n :: Nat) s = Printfn s (TupleListLax n)

type Printf2 (s :: Symbol) = Printfn s '(Fst,'(Snd, '()))
-- Printf3/Printf3' expected format is (a, (b,c)) : we dont support (a,b,c) ever!
type Printf3 (s :: Symbol) = Printfn s '(Fst, '(Snd >> Fst, '(Snd >> Snd, '())))
type Printf3' (s :: Symbol) = Printfn s (TupleI '[Fst, Snd >> Fst, Snd >> Snd])

instance (KnownNat (TupleLenT as)
        , PrintC bs
        , (b,bs) ~ ReverseTupleP (a,as)
        , ReverseTupleC (a,as)
        , Show a
        , Show as
        , PrintfArg b
        , PP s x ~ String
        , PP p x ~ (a,as)
        , P s x
        , P p x
        , CheckT (PP p x) ~ 'True
        ) => P (Printfn s p) x where
  type PP (Printfn s p) x = String
  eval _ opts x = do
    let msg0 = "Printfn"
    lrx <- runPQ msg0 (Proxy @s) (Proxy @p) opts x
    case lrx of
      Left e -> pure e
      Right (s,(a,as),ss,pp) -> do
        let len :: Int = 1 + nat @(TupleLenT as)
            msg1 = msg0 <> "(" <> show len <> ")"
            hhs = [hh ss, hh pp]
        lr <- catchitNF @_ @E.SomeException (prtC @bs s (reverseTupleC (a,as)))
        pure $ case lr of
          Left e -> mkNode opts (FailT (msg1 <> "(" <> e <> ")")) [msg1 <> show0 opts " " a <> " s=" <> s] hhs
          Right ret -> mkNode opts (PresentT ret) [msg1 <> " [" <> showLit0 opts "" ret <> "]" <> showA opts " | (a,as)=" (a,as) <> showLit0 opts " | s=" s] hhs

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
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst <$ Snd) ("abc",Just 20)
--   Present Just "abc"
--   PresentT (Just "abc")
--
data p <$ q
infixl 4 <$

instance (P p a
        , P q a
        , Show (PP p a)
        , Functor t
        , PP q a ~ t c
        , ApplyConstT (PP q a) (PP p a) ~ t (PP p a)
        ) => P (p <$ q) a where
  type PP (p <$ q) a = ApplyConstT (PP q a) (PP p a)
  eval _ opts a = do
    let msg0 = "(<$)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <$ q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " p] [hh pp, hh qq]

data p <* q
infixl 4 <*

-- | similar to 'Control.Applicative.<*'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst <* Snd) (Just "abc",Just 20)
--   Present Just "abc"
--   PresentT (Just "abc")
--
type p *> q = q <* p
infixl 4 *>

instance (Show (t c)
        , P p a
        , P q a
        , Show (t b)
        , Applicative t
        , t b ~ PP p a
        , PP q a ~ t c
        ) => P (p <* q) a where
  type PP (p <* q) a = PP p a
  eval _ opts a = do
    let msg0 = "(<*)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <* q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " p <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]

-- | similar to 'Control.Applicative.<|>'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Fst <|> Snd) (Nothing,Just 20)
--   Present Just 20
--   PresentT (Just 20)
--
--   >>> pl @(Fst <|> Snd) (Just 10,Just 20)
--   Present Just 10
--   PresentT (Just 10)
--
--   >>> pl @(Fst <|> Snd) (Nothing,Nothing)
--   Present Nothing
--   PresentT Nothing
--
data p <|> q
infixl 3 <|>

instance (P p a
        , P q a
        , Show (t b)
        , Alternative t
        , t b ~ PP p a
        , PP q a ~ t b
        ) => P (p <|> q) a where
  type PP (p <|> q) a = PP p a
  eval _ opts a = do
    let msg0 = "(<|>)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let d = p <|> q
        in mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | p=" p <> showA opts " | q=" q] [hh pp, hh qq]


-- | similar to 'Control.Comonad.extract'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Extract (Nothing,Just 20)
--   Present Just 20
--   PresentT (Just 20)
--
--   >>> pl @Extract (Identity 20)
--   Present 20
--   PresentT 20
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
    in pure $ mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | " ta] []

-- | similar to 'Control.Comonad.duplicate'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Duplicate (20,"abc")
--   Present (20,(20,"abc"))
--   PresentT (20,(20,"abc"))
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
    in pure $ mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | " ta] []

-- | similar to 'Control.Monad.join'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @Join  (Just (Just 20))
--   Present Just 20
--   PresentT (Just 20)
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
    in pure $ mkNode opts (PresentT d) [msg0 <> show0 opts " " d <> showA opts " | " tta] []

-- same as $ but shows 'a' and 'b'
data p $ q
infixl 0 $

type p & q = q $ p -- flips the args eg a & b & (,) = (b,a)
infixr 1 &

instance (P p x
        , P q x
        , PP p x ~ (a -> b)
        , FnT (PP p x) ~ b
        , PP q x ~ a
        , Show a
        , Show b
        ) => P (p $ q) x where
  type PP (p $ q) x = FnT (PP p x)
  eval _ opts x = do
    let msg0 = "($)"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq)  ->
        let d = p q
        in mkNode opts (PresentT d) ["fn $ " <> show q <> " = " <> show d] [hh pp, hh qq]

type family FnT ab :: Type where
  FnT (a -> b) = b
  FnT ab = GL.TypeError (
      'GL.Text "FnT: expected Type -> Type but found a simple Type?"
      ':$$: 'GL.Text "ab = "
      ':<>: 'GL.ShowType ab)

evalQuick :: forall p i . P p i => i -> Either String (PP p i)
evalQuick i = getValLRFromTT (runIdentity (eval (Proxy @p) o0 i))

-- | similar to 'T.strip' 'T.stripStart' 'T.stripEnd'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(Trim Snd) (20," abc   " :: String)
--   Present "abc"
--   PresentT "abc"
--
--   >>> import Data.Text (Text)
--   >>> pl @(Trim Snd) (20," abc   " :: Text)
--   Present "abc"
--   PresentT "abc"
--
--   >>> pl @(TrimStart Snd) (20," abc   ")
--   Present "abc   "
--   PresentT "abc   "
--
--   >>> pl @(TrimEnd Snd) (20," abc   ")
--   Present " abc"
--   PresentT " abc"
--
-- todo: make it work for 'Data.Text.Lens.IsText'
data Trim' (left :: Bool) (right :: Bool) p
type Trim p = Trim' 'True 'True p
type TrimStart p = Trim' 'True 'False p
type TrimEnd p = Trim' 'False 'True p

instance (FailIfT (NotT (OrT l r))
           ('GL.Text "Trim': left and right cannot both be False")
        , GetBool l
        , GetBool r
        , IsText (PP p x)
        , P p x
        ) => P (Trim' l r p) x where
  type PP (Trim' l r p) x = PP p x
  eval _ opts x = do
    let msg0 = "Trim" ++ (if l && r then "" else if l then "Start" else "End")
        l = getBool @l
        r = getBool @r
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right (view unpacked -> p) ->
        let fl = if l then dropWhile isSpace else id
            fr = if r then dropWhileEnd isSpace else id
            b =  (fl . fr) p
        in mkNode opts (PresentT (b ^. packed)) [msg0 <> showLit0 opts "" b <> showLit opts " | " p] [hh pp]

-- | similar to 'T.stripLeft' 'T.stripRight'
--
--   >>> :set -XTypeApplications
--   >>> :set -XDataKinds
--   >>> pl @(StripLeft "xyz" Id) ("xyzHello" :: String)
--   Present Just "Hello"
--   PresentT (Just "Hello")
--
--   >>> import Data.Text (Text)
--   >>> pl @(StripLeft "xyz" Id) ("xyzHello" :: Text)
--   Present Just "Hello"
--   PresentT (Just "Hello")
--
--   >>> pl @(StripLeft "xyz" Id) "xywHello"
--   Present Nothing
--   PresentT Nothing
--
--   >>> pl @(StripRight "xyz" Id) "Hello xyz"
--   Present Just "Hello "
--   PresentT (Just "Hello ")
--
--   >>> pl @(StripRight "xyz" Id) "xyzHelloxyw"
--   Present Nothing
--   PresentT Nothing
--
data StripLR (right :: Bool) p q
type StripRight p q = StripLR 'True p q
type StripLeft p q = StripLR 'False p q

instance (GetBool r
        , PP p x ~ String
        , P p x
        , IsText (PP q x)
        , P q x
        ) => P (StripLR r p q) x where
  type PP (StripLR r p q) x = Maybe (PP q x)
  eval _ opts x = do
    let msg0 = "Strip" ++ (if r then "Right" else "Left")
        r = getBool @r
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x
    pure $ case lr of
      Left e -> e
      Right (p,view unpacked -> q,pp,qq) ->
        let b = if r then
                  let (before,after) = splitAt (length q - length p) q
                  in if after == p then Just before else Nothing
                else
                  let (before,after) = splitAt (length p) q
                  in if before == p then Just after else Nothing
        in mkNode opts (PresentT (fmap (view packed) b)) [msg0 <> show0 opts "" b <> showLit opts " | p=" p <> showLit opts " | q=" q] [hh pp, hh qq]


-- PP is k -> * -> * but we need [k]: need to unwrap into [k]
data RepeatP (n :: Nat) p
instance (P (RepeatT n p) x) => P (RepeatP n p) x where
  type PP (RepeatP n p) x = PP (RepeatT n p) x
  eval _ opts x =
    eval (Proxy @(RepeatT n p)) opts x

