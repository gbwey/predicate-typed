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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{- |
     Utility methods for Predicate / methods for displaying the evaluation tree ...
-}
module Predicate.Util (
  -- ** TT
    TT(..)
  , tBool
  , tStrings
  , tForest
  , fixBoolT
  , topMessage
  , topMessage'
  , hasNoTree

 -- ** BoolT
  , BoolT(..)
  , _FailT
  , _PresentT
  , _FalseT
  , _TrueT

 -- ** BoolP
  , boolT2P
  , BoolP
  , PE(PE)
  , pStrings
  , pBool

 -- ** create tree functions
  , mkNode
  , mkNodeB
  , mkNodeSkipP

 -- ** tree manipulation
  , getValLRFromTT
  , getValLR
  , fromTT
  , getValueLR
  , getValueLRHide
  , fixLite
  , fixit
  , prefixMsg
  , splitAndAlign

 -- ** display options
  , POpts(..)
  , ODebug(..)
  , defOpts
  , oz
  , ol
  , olc
  , o0
  , o2
  , o2n
  , o3
  , ou
  , ou3
  , oun
  , setw
  , setu
  , setc
  , color0
  , color1
  , color2
  , color3
  , color4
  , colorMe
  , zero
  , lite
  , subnormal
  , normal
  , verbose
  , isVerbose
  , ansi
  , unicode
  , showBoolP

-- ** formatting functions
  , show01
  , lit01
  , show01'
  , lit01'
  , showLit0
  , showLit1
  , show0
  , show3
  , show1

  -- ** regular expressions
  , ROpt(..)
  , compileRegex
  , GetROpts(..)
  , RR(..)

  -- ** useful type families
  , BetweenT
  , NullT
  , FailWhenT
  , FailUnlessT
  , AndT
  , OrT
  , NotT
  , RepeatT
  , IntersperseT
  , LenT
  , InductTupleC(..)
  , InductListC(..)
  , FlipT
  , IfT
  , SumT
  , MapT
  , ConsT
  , type (%%)
  , type (%&)
  , T_1
  , T_2
  , T_3
  , T_4

 -- ** extract values from the type level
  , nat
  , symb
  , GetNats(..)
  , GetSymbs(..)
  , getLen
  , GetLen(..)
  , showThese
  , GetThese(..)
  , GetOrdering(..)
  , GetBool(..)
  , OrderingP(..)
  , GetOrd(..)

 -- ** printing methods
  , prtTTIO
  , prtTT
  , prtTree
  , prtImpl
  , prtTreePure
  , prettyRational

 -- ** boolean methods
  , (~>)

 -- ** miscellaneous
  , Holder
  , hh
  , showT
  , prettyOrd
  , removeAnsi
  , MonadEval(..)
    ) where
import qualified GHC.TypeNats as GN
import Data.Ratio
import GHC.TypeLits (Symbol,Nat,KnownSymbol,KnownNat,ErrorMessage((:$$:),(:<>:)))
import qualified GHC.TypeLits as GL
import Control.Lens
import Control.Arrow
import Data.List
import qualified Data.Tree.View as TV
import Data.Tree
import Data.Tree.Lens
import Data.Proxy
import Data.Char
import Data.Data
import System.Console.Pretty
import GHC.Exts (Constraint)
import qualified Text.Regex.PCRE.Heavy as RH
import qualified Text.Regex.PCRE.Light as RL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Data.ByteString (ByteString)
import GHC.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative (ZipList)
import Data.Kind (Type)
import Data.These
import Data.These.Combinators
import qualified Control.Exception as E
import Control.DeepSeq
import System.IO.Unsafe (unsafePerformIO)
import Data.Bool
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
--import Data.Maybe
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XNoStarIsType

-- | represents the evaluation tree for predicates
data TT a = TT { _tBool :: BoolT a  -- ^ the value at this root node
               , _tStrings :: [String]  -- ^ detailed information eg input and output and text
               , _tForest :: Forest PE } -- ^ the child nodes
                deriving Show

-- | contains the typed result from evaluating the expression tree
--
data BoolT a where
  FailT :: String -> BoolT a  -- ^ failure with string
  FalseT :: BoolT Bool        -- ^ false predicate
  TrueT :: BoolT Bool         -- ^ true predicate
  PresentT :: a -> BoolT a    -- ^ non predicate value

deriving instance Show a => Show (BoolT a)
deriving instance Eq a => Eq (BoolT a)

-- | lens for accessing 'BoolT' in 'TT'
tBool :: Lens (TT a) (TT b) (BoolT a) (BoolT b)
tBool afb s = (\b -> s { _tBool = b }) <$> afb (_tBool s)

tStrings :: Lens' (TT a) [String]
tStrings afb s = (\b -> s { _tStrings = b }) <$> afb (_tStrings s)

tForest :: Lens' (TT a) (Forest PE)
tForest afb s = (\b -> s { _tForest = b }) <$> afb (_tForest s)

-- | a lens from typed 'BoolT' to the untyped 'BoolP'
boolT2P :: Lens' (BoolT a) BoolP
boolT2P afb = \case
  FailT e -> FailT e <$ afb (FailP e)
  TrueT -> TrueT <$ afb TrueP
  FalseT -> FalseT <$ afb FalseP
  PresentT a -> PresentT a <$ afb PresentP

-- | contains the untyped result from evaluating the expression tree
data BoolP =
    FailP String -- ^ fails the entire evaluation
  | FalseP       -- ^ False predicate
  | TrueP        -- ^ True predicate
  | PresentP     -- ^ Any value
  deriving (Show, Eq)

data PE = PE { _pBool :: BoolP -- ^ holds the result of running the predicate
             , _pStrings :: [String] -- ^ optional strings to include in the results
             } deriving Show

pBool :: Lens' PE BoolP
pBool afb (PE x y) = flip PE y <$> afb x

pStrings :: Lens' PE [String]
pStrings afb s = (\b -> s { _pStrings = b }) <$> afb (_pStrings s)

-- | creates a Node for the evaluation tree
mkNode :: POpts -> BoolT a -> [String] -> [Holder] -> TT a
mkNode opts bt ss hs =
  case oDebug opts of
    OZero -> TT bt [] []
    OLite -> TT bt (take 1 ss) [] -- keeps the last one so we can use the root to give more details on failure (especially for Refined and Refined3 types)
    _ -> TT bt ss (map fromTTH hs)

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts -> Bool -> [String] -> [Holder] -> TT Bool
mkNodeB opts b = mkNode opts (bool FalseT TrueT b)

mkNodeSkipP :: Tree PE
mkNodeSkipP = Node (PE TrueP ["skipped PP ip i = Id"]) []

partitionTTExtended :: (w, TT a) -> ([((w, TT x), String)], [(w, TT a)])
partitionTTExtended z@(_, t) =
  case _tBool t of
    FailT e -> ([(z & _2 . tBool .~ FailT e, e)], [])
    _ -> ([], [z])

getValLRFromTT :: TT a -> Either String a
getValLRFromTT = getValLR  . _tBool

-- | get the value from BoolT or fail
getValLR :: BoolT a -> Either String a
getValLR = \case
    FailT e -> Left e
    TrueT -> Right True
    FalseT -> Right False
    PresentT a -> Right a

-- | converts a typed tree to an untyped on for display
fromTT :: TT a -> Tree PE
fromTT (TT bt ss tt) = Node (PE (bt ^. boolT2P) ss) tt

-- | a monomorphic container of trees
data Holder = forall w . Holder (TT w)

-- | converts a typed tree into an untyped one
fromTTH :: Holder -> Tree PE
fromTTH (Holder x) = fromTT x

-- | convenience method to wrap a typed tree
hh :: TT w -> Holder
hh = Holder

-- | see 'getValueLRImpl' : add more detail to the tree if there are errors
getValueLR :: POpts -> String -> TT a -> [Holder] -> Either (TT x) a
getValueLR = getValueLRImpl True

-- | see 'getValueLRImpl' : add less detail to the tree if there are errors
getValueLRHide :: POpts -> String -> TT a -> [Holder] -> Either (TT x) a
getValueLRHide = getValueLRImpl False

-- elide FailT msg in tStrings[0] if showError is False
-- | a helper method to add extra context on failure to the tree or extract the value at the root of the tree
getValueLRImpl :: Bool -> POpts -> String -> TT a -> [Holder] -> Either (TT x) a
getValueLRImpl showError opts msg0 tt hs =
  let tt' = hs ++ [hh tt]
  in left (\e -> mkNode
                   opts
                  (FailT e)
                   [msg0 <> if showError then (if null msg0 then "" else " ") <> "[" <> e <> "]"
                            else ""]
                  tt'
          )
          (getValLRFromTT tt)

-- | the color palette for displaying the expression tree
newtype PColor = PColor (BoolP -> String -> String)

-- | customizable options
data POpts = POpts { oWidth :: Int -- ^ length of data to display for 'showLitImpl'
                   , oDebug :: !ODebug -- ^ debug level
                   , oDisp :: Disp -- ^ display the tree using the normal tree or unicode
                   , oColor :: !(String, PColor) -- ^ color palette used
                   }

-- | display format for the tree
data Disp = Ansi -- ^ draw normal tree
          | Unicode  -- ^ use unicode
          deriving (Show, Eq)

instance Show POpts where
  show opts =
    "POpts: showA=" <> show (oWidth opts)
    <> " debug=" <> show (oDebug opts)
    <> " disp=" <> show (oDisp opts)
    <> " color=" <> show (fst (oColor opts))

defOpts :: POpts
defOpts = POpts
    { oWidth = 200
    , oDebug = ONormal
    , oDisp = Ansi
    , oColor = color1
    }

data ODebug =
       OZero
     | OLite
     | OSubNormal
     | ONormal
     | OVerbose
     deriving (Ord, Show, Eq, Enum, Bounded)

-- | skip colors and just return the summary
oz :: POpts
oz = defOpts { oColor = color0, oDebug = OZero }

-- | skip colors and just return the summary
ol :: POpts
ol = defOpts { oColor = color0, oDebug = OLite }

-- | skip the detail and just return the summary but keep the colors
olc :: POpts
olc = ol { oColor = color1 }

-- | displays the detailed evaluation tree without colors.
o0 :: POpts
o0 = defOpts { oColor = color0 }

-- | displays the detailed evaluation tree using colors.
o2 :: POpts
o2 = defOpts

-- | same as 'o2' but for a narrow display
o2n :: POpts
o2n = o2 { oWidth = 120 }

-- | same as 'o2' for a wider display and verbose debug mode setting
o3 :: POpts
o3 = defOpts { oDebug = OVerbose, oWidth = 400 }

-- | displays the detailed evaluation tree using unicode and colors. ('o2' works better on Windows)
ou :: POpts
ou = defOpts { oDisp = Unicode }

-- | same as 'ou' for a wider display and verbose debug mode setting
ou3 :: POpts
ou3 = o3 { oDisp = Unicode }

-- | same as 'ou' but for a narrow display
oun :: POpts
oun = ou { oWidth = 120 }

-- | helper method to set the debug level
isVerbose :: POpts -> Bool
isVerbose = (OVerbose==) . oDebug

-- | helper method to limit the width of the tree
setw :: Int -> POpts -> POpts
setw w o = o { oWidth = w }

-- | helper method to set the debug level
verbose :: POpts -> POpts
verbose o = o { oDebug = OVerbose }

-- | helper method to set the debug level
normal :: POpts -> POpts
normal o = o { oDebug = ONormal }

-- | helper method to set the debug level
subnormal :: POpts -> POpts
subnormal o = o { oDebug = OSubNormal }

-- | set display to unicode and colors
setu :: POpts -> POpts
setu o = o { oDisp = Unicode }

-- | set a color palette
setc :: (String, PColor) -> POpts -> POpts
setc pc o = o { oColor = pc }

-- | color palettes
--
-- italics dont work but underline does
color0, color1, color2, color3, color4 :: (String, PColor)

-- | no colors are displayed
color0 = ("color0", PColor $ flip const)

-- | default color palette
color1 =
  ("color1",) $ PColor $ \case
    FailP {} -> bgColor Magenta
    FalseP -> bgColor Red
    TrueP -> bgColor Green
    PresentP -> bgColor Yellow

color2 =
  ("color2",) $ PColor $ \case
    FailP {} -> bgColor Magenta
    FalseP -> bgColor Red
    TrueP -> bgColor White
    PresentP -> bgColor Yellow

color3 =
  ("color3",) $ PColor $ \case
    FailP {} -> bgColor Blue
    FalseP -> color Red
    TrueP -> color White
    PresentP -> bgColor Yellow

color4 =
  ("color4",) $ PColor $ \case
    FailP {} -> bgColor Cyan
    FalseP -> color Red
    TrueP -> color Green
    PresentP -> bgColor Yellow

-- | fix PresentT Bool to TrueT or FalseT
fixBoolT :: TT Bool -> TT Bool
fixBoolT t =
  case t ^? tBool . _PresentT of
    Nothing -> t
    Just b -> t & tBool .~ _boolT # b

show01 :: (Show a1, Show a2) => POpts -> String -> a1 -> a2 -> String
show01 opts msg0 ret as = lit01 opts msg0 ret (show as)

lit01 :: Show a1 => POpts -> String -> a1 -> String -> String
lit01 opts msg0 ret as = lit01' opts msg0 ret "" as

show01' :: (Show a1, Show a2) => POpts -> String -> a1 -> String -> a2 -> String
show01' opts msg0 ret fmt as = lit01' opts msg0 ret fmt (show as)

lit01' :: Show a1 => POpts -> String -> a1 -> String -> String -> String
lit01' opts msg0 ret fmt as = msg0 <> show0 opts " " ret <> showLit1 opts (" | " ++ fmt) as

-- | display all data regardless of debug level
showLit0 :: POpts -> String -> String -> String
showLit0 o s a = showLitImpl o OLite s a

-- | more restrictive: only display data at debug level 1 or less
showLit1 :: POpts -> String -> String -> String
showLit1 o s a = showLitImpl o OLite s a

showLitImpl :: POpts -> ODebug -> String -> String -> String
showLitImpl o i s a =
  if oDebug o >= i then
    let f n = let ss = take n a
              in ss <> (if length ss==n then " ..." else "")
    in s <> f (oWidth o)
  else ""

show0 :: Show a => POpts -> String -> a -> String
show0 o s a = showAImpl o OLite s a

show3 :: Show a => POpts -> String -> a -> String
show3 o s a = showAImpl o OVerbose s a

show1 :: Show a => POpts -> String -> a -> String
show1 o s a = showAImpl o OLite s a

showAImpl :: Show a => POpts -> ODebug -> String -> a -> String
showAImpl o i s a = showLitImpl o i s (show a)

-- | Regex options for Rescan Resplit Re etc
data ROpt =
    Anchored -- ^ Force pattern anchoring
  | Auto_callout -- ^ Compile automatic callouts
--  | Bsr_anycrlf --  \R matches only CR, LF, or CrlF
--  | Bsr_unicode -- ^ \R matches all Unicode line endings
  | Caseless -- ^ Do caseless matching
  | Dollar_endonly -- ^ dollar not to match newline at end
  | Dotall -- ^ matches anything including NL
  | Dupnames -- ^ Allow duplicate names for subpatterns
  | Extended -- ^ Ignore whitespace and # comments
  | Extra -- ^ PCRE extra features (not much use currently)
  | Firstline -- ^ Force matching to be before newline
  | Multiline -- ^ caret and dollar match newlines within data
--  | Newline_any -- ^ Recognize any Unicode newline sequence
--  | Newline_anycrlf -- ^ Recognize CR, LF, and CrlF as newline sequences
  | Newline_cr -- ^ Set CR as the newline sequence
  | Newline_crlf -- ^ Set CrlF as the newline sequence
  | Newline_lf -- ^ Set LF as the newline sequence
  | No_auto_capture -- ^ Disable numbered capturing parentheses (named ones available)
  | Ungreedy -- ^ Invert greediness of quantifiers
  | Utf8 -- ^ Run in UTF--8 mode
  | No_utf8_check -- ^ Do not check the pattern for UTF-8 validity
  deriving (Show,Eq,Ord,Enum,Bounded)

-- | compile a regex using the type level symbol
compileRegex :: forall rs a . GetROpts rs
  => POpts -> String -> String -> [Holder] -> Either (TT a) RH.Regex
compileRegex opts nm s hhs =
    let rs = getROpts @rs
        mm = nm <> " " <> show rs
    in flip left (RH.compileM (B8.pack s) rs)
          $ \e -> mkNode opts (FailT "Regex failed to compile") [mm <> " compile failed with regex msg[" <> e <> "]"] hhs

-- | extract the regex options from the type level list
class GetROpts (os :: [ROpt]) where
  getROpts :: [RL.PCREOption]
instance GetROpts '[] where
  getROpts = []
instance (GetROpt r, GetROpts rs) => GetROpts (r ': rs) where
  getROpts = getROpt @r : getROpts @rs

-- | convert type level regex option to the value level
class GetROpt (o :: ROpt) where
  getROpt :: RL.PCREOption
instance GetROpt 'Anchored where getROpt = RL.anchored
instance GetROpt 'Auto_callout where getROpt = RL.auto_callout
--instance GetROpt 'Bsr_anycrlf where getROpt = RL.bsr_anycrlf
--instance GetROpt 'Bsr_unicode where getROpt = RL.bsr_unicode
instance GetROpt 'Caseless where getROpt = RL.caseless
instance GetROpt 'Dollar_endonly where getROpt = RL.dollar_endonly
instance GetROpt 'Dotall where getROpt = RL.dotall
instance GetROpt 'Dupnames where getROpt = RL.dupnames
instance GetROpt 'Extended where getROpt = RL.extended
instance GetROpt 'Extra where getROpt = RL.extra
instance GetROpt 'Firstline where getROpt = RL.firstline
instance GetROpt 'Multiline where getROpt = RL.multiline
--instance GetROpt 'Newline_any where getROpt = RL.newline_any
--instance GetROpt 'Newline_anycrlf where getROpt = RL.newline_anycrlf
instance GetROpt 'Newline_cr where getROpt = RL.newline_cr
instance GetROpt 'Newline_crlf where getROpt = RL.newline_crlf
instance GetROpt 'Newline_lf where getROpt = RL.newline_lf
instance GetROpt 'No_auto_capture where getROpt = RL.no_auto_capture
instance GetROpt 'Ungreedy where getROpt = RL.ungreedy
instance GetROpt 'Utf8 where getROpt = RL.utf8
instance GetROpt 'No_utf8_check where getROpt = RL.no_utf8_check

-- | used by 'Predicate.ReplaceImpl' and 'RH.sub' and 'RH.gsub' to allow more flexible replacement
--   These parallel the RegexReplacement (not exported) class in "Text.Regex.PCRE.Heavy" but have overlappable instances which is problematic for this code so I use 'RR'
data RR =
     RR String
   | RR1 (String -> [String] -> String)
   | RR2 (String -> String)
   | RR3 ([String] -> String)

instance Show RR where
  show = \case
           RR s -> "RR " ++ s
           RR1 {} -> "RR1 <fn>"
           RR2 {} -> "RR2 <fn>"
           RR3 {} -> "RR3 <fn>"

-- | extract values from the trees or if there are errors returned a tree with added context
splitAndAlign :: Show x =>
                    POpts
                    -> [String]
                    -> [((Int, x), TT a)]
                    -> Either (TT w)
                              ([a]
                              ,[((Int, x), TT a)]
                              )
splitAndAlign opts msgs ts =
  case mconcat $ map partitionTTExtended ts of
     (excs@(e:_), _) ->
          Left $ mkNode opts
                       (FailT (groupErrors (map snd excs)))
                       (msgs <> ["excs=" <> show (length excs) <> " " <> formatList opts [fst e]])
                       (map (hh . snd) ts)
     ([], tfs) -> Right (valsFromTTs (map snd ts), tfs)

formatList :: forall x z . Show x => POpts -> [((Int, x), z)] -> String
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showAImpl opts OLite ", a=" a <> ")")

-- | extract all root values from a list of trees
valsFromTTs :: [TT a] -> [a]
valsFromTTs = concatMap toList

instance Foldable TT where
  foldMap am = foldMap am . _tBool

instance Foldable BoolT where
  foldMap am = either (const mempty) am . getValLR

-- cant use: is / isn't / has as only FailT will be False: use Fold
-- this is more specific to TrueP FalseP
-- | prism from BoolT to Bool
_boolT :: Prism' (BoolT Bool) Bool
_boolT = prism' (bool FalseT TrueT)
         $ \case
              PresentT a -> Just a
              TrueT -> Just True
              FalseT -> Just False
              FailT {} -> Nothing

groupErrors :: [String] -> String
groupErrors =
     intercalate " | "
   . map (\xs@(x :| _) -> x <> (if length xs > 1 then "(" <> show (length xs) <> ")" else ""))
   . N.group

_FailT :: Prism' (BoolT a) String
_FailT = prism' FailT $ \case
                         FailT s -> Just s
                         _ -> Nothing

_PresentT :: Prism' (BoolT a) a
_PresentT = prism' PresentT $ \case
                                PresentT a -> Just a
                                _ -> Nothing

_FalseT :: Prism' (BoolT Bool) ()
_FalseT = prism' (const FalseT) $
            \case
               FalseT -> Just ()
               _ -> Nothing

_TrueT :: Prism' (BoolT Bool) ()
_TrueT = prism' (const TrueT) $
            \case
               TrueT -> Just ()
               _ -> Nothing

-- | boolean implication
--
-- >>> True ~> False
-- False
--
-- >>> True ~> True
-- True
--
-- >>> False ~> False
-- True
--
-- >>> False ~> True
-- True

(~>) :: Bool -> Bool -> Bool
p ~> q = not p || q

-- | type level Between
type family BetweenT (a :: Nat) (b :: Nat) (v :: Nat) :: Constraint where
  BetweenT m n v =
     FailUnlessT (AndT (m GL.<=? v) (v GL.<=? n))
            ('GL.Text "BetweenT failure"
             ':$$: 'GL.ShowType v
             ':$$: 'GL.Text " is outside of "
             ':$$: 'GL.ShowType m
             ':<>: 'GL.Text " and "
             ':<>: 'GL.ShowType n)

-- | typelevel Null on Symbol
type family NullT (x :: Symbol) :: Bool where
  NullT ("" :: Symbol) = 'True
  NullT _ = 'False

-- | helper method to fail with an error when True
type family FailWhenT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailWhenT 'False _ = ()
  FailWhenT 'True e = GL.TypeError e

-- | helper method to fail with an error when False
type family FailUnlessT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailUnlessT 'True _ = ()
  FailUnlessT 'False e = GL.TypeError e

-- | typelevel And
type family AndT (b :: Bool) (b1 :: Bool) :: Bool where
  AndT 'False _ = 'False
  AndT 'True b1 = b1

-- | typelevel Or
type family OrT (b :: Bool) (b1 :: Bool) :: Bool where
  OrT 'True _ = 'True
  OrT 'False b1 = b1

-- | typelevel Not
type family NotT (b :: Bool) :: Bool where
  NotT 'True = 'False
  NotT 'False = 'True

-- | get a Nat from the typelevel
--
-- >>> nat @14
-- 14
--
nat :: forall n a . (KnownNat n, Num a) => a
nat = fromIntegral (GL.natVal (Proxy @n))

-- | gets the Symbol from the typelevel
--
-- >>> symb @"abc"
-- "abc"
--
symb :: forall s . KnownSymbol s => String
symb = GL.symbolVal (Proxy @s)

-- | get a list of Nats from the typelevel
--
-- >>> getNats @'[10,12,1]
-- [10,12,1]
class GetNats as where
  getNats :: [Int]
instance GetNats '[] where
  getNats = []
instance (KnownNat n, GetNats ns) => GetNats (n ': ns) where
  getNats = nat @n : getNats @ns

-- | get a list of Symbols from the typelevel
--
-- >>> getSymbs @'["abc","def","g"]
-- ["abc","def","g"]
--
class GetSymbs ns where
  getSymbs :: [String]
instance GetSymbs '[] where
  getSymbs = []
instance (KnownSymbol s, GetSymbs ss) => GetSymbs (s ': ss) where
  getSymbs = symb @s : getSymbs @ss

-- | get the length of a typelevel list
--
-- >>> getLen @'["abc","def","g"]
-- 3
--
getLen :: forall xs . GetLen xs => Int
getLen = getLenP (Proxy @xs)

-- really need a proxy for this to work
-- | gets length of a typelevel list
class GetLen (xs :: [k]) where  -- defaults to xs :: k (how to make it [k]) cos is not free
  getLenP :: Proxy (xs :: [k]) -> Int
instance GetLen '[] where
  getLenP _ = 0
instance GetLen xs => GetLen (x ': xs) where
  getLenP _ = 1 + getLenP (Proxy @xs)

showThese :: These a b -> String
showThese = these (const "This") (const "That") (const (const "These"))

-- hard without a Proxy
class GetThese (th :: These x y) where
  getThese :: Proxy th -> (String, These w v -> Bool)
instance GetThese ('This x) where
  getThese _ = ("This", isThis)
instance GetThese ('That y) where
  getThese _ = ("That", isThat)
instance GetThese ('These x y) where
  getThese _ = ("These", isThese)

-- | get ordering from the typelevel
class GetOrdering (cmp :: Ordering) where
  getOrdering :: Ordering
instance GetOrdering 'LT where
  getOrdering = LT
instance GetOrdering 'EQ where
  getOrdering = EQ
instance GetOrdering 'GT where
  getOrdering = GT

-- | get bool from the typelevel
class GetBool (a :: Bool) where
  getBool :: Bool
instance GetBool 'True where
  getBool = True
instance GetBool 'False where
  getBool = False

data OrderingP = Cgt | Cge | Ceq | Cle | Clt | Cne deriving (Show, Eq, Enum, Bounded)

class GetOrd (k :: OrderingP) where
  getOrd :: Ord a => (String, a -> a -> Bool)

instance GetOrd 'Cgt where getOrd = (">", (>))
instance GetOrd 'Cge where getOrd = (">=",(>=))
instance GetOrd 'Ceq where getOrd = ("==",(==))
instance GetOrd 'Cle where getOrd = ("<=",(<=))
instance GetOrd 'Clt where getOrd = ("<", (<))
instance GetOrd 'Cne where getOrd = ("/=",(/=))

toNodeString :: POpts -> PE -> String
toNodeString opts bpe =
  if hasNoTree opts then error $ "shouldnt be calling this if we are dropping details: toNodeString " <> show (oDebug opts) <> " " <> show bpe
  else showBoolP opts (_pBool bpe) <> " " <> displayMessages (_pStrings bpe)

hasNoTree :: POpts -> Bool
hasNoTree opts =
  case oDebug opts of
    OZero -> True
    OLite -> True
    OSubNormal -> False
    ONormal -> False
    OVerbose -> False

nullSpace :: String -> String
nullSpace s | null s = ""
            | otherwise = " " <> s

showBoolP :: POpts -> BoolP -> String
showBoolP o =
  \case
    b@(FailP e) -> "[" <> colorMe o b "Error" <> nullSpace e <> "]"
    b@PresentP -> colorMe o b "P"
    b@TrueP -> colorMe o b "True "
    b@FalseP -> colorMe o b "False"

displayMessages :: [String] -> String
displayMessages es =
  case filter (not . all isSpace) es of
    [] -> ""
    z -> intercalate " | " z

-- | colors the result of the predicate based on the current color palette
colorMe :: POpts -> BoolP -> String -> String
colorMe o b s =
  let (_, PColor f) = oColor o
  in f b s

prtTTIO :: POpts -> IO (TT a) -> IO ()
prtTTIO  = prtTT'

prtTT :: POpts -> Identity (TT a) -> IO ()
prtTT  = prtTT'

prtTT' :: MonadEval m => POpts -> m (TT a) -> IO ()
prtTT' o y = liftEval y >>= prtTree o . fromTT

prtTree :: POpts -> Tree PE -> IO ()
prtTree o = putStr . prtTreePure o -- prtImpl o . fmap (toNodeString o)

prtImpl :: POpts -> Tree String -> IO ()
prtImpl = (putStr .) . showImpl

fixLite :: forall a . Show a => POpts -> a -> Tree PE -> String
fixLite opts a t
  | hasNoTree opts = fixPresentP opts (t ^. root . pBool) a <> "\n"
  | otherwise = prtTreePure opts t

fixPresentP :: Show a => POpts -> BoolP -> a -> String
fixPresentP opts bp a =
  case bp of
    PresentP -> colorMe opts PresentP "Present" <> " " <> show a
    _ -> showBoolP opts bp

prtTreePure :: POpts -> Tree PE -> String
prtTreePure opts t
  | hasNoTree opts = showBoolP opts (t ^. root . pBool)
  | otherwise = showImpl opts $ fmap (toNodeString opts) t

topMessage' :: TT a -> String
topMessage' pp = maybe "" innermost (pp ^? tStrings . ix 0)

topMessage :: TT a -> String
topMessage pp = maybe "" (\x -> "(" <> x <> ")") (pp ^? tStrings . ix 0)

innermost :: String -> String
innermost = ('{':) . reverse . ('}':) . takeWhile (/='{') . dropWhile (=='}') . reverse

showImpl :: POpts -> Tree String -> String
showImpl o =
  case oDisp o of
    Unicode -> TV.showTree
    Ansi -> drawTree -- to drop the last newline else we have to make sure that everywhere else has that newline: eg fixLite

-- | skip displaying the tree and just output the result
lite :: POpts -> POpts
lite o = o { oDebug = OLite }

zero :: POpts -> POpts
zero o = o { oDebug = OZero }

-- | display in unicode (non-Windows)
unicode :: POpts -> POpts
unicode o = o { oDisp = Unicode }

-- | normal display
ansi :: POpts -> POpts
ansi o = o { oDisp = Ansi }

prettyRational :: Rational -> String
prettyRational (numerator &&& denominator -> (n,d)) =
  if | n == 0 -> "0"
     | d == 1 -> show n
     | otherwise -> show n <> " / " <> show d

fixit :: ((Int, x), TT a) -> TT a
fixit ((i, _), t) = prefixMsg ("i=" <> show i <> ":") t

prefixMsg :: String -> TT a -> TT a
prefixMsg msg t =
   t & tStrings . ix 0 %~ (msg <>)

showT :: forall (t :: Type) . Typeable t => String
showT = show (typeRep (Proxy @t))

prettyOrd :: Ordering -> String
prettyOrd = \case
              LT -> "<"
              EQ -> "="
              GT -> ">"

type family RepeatT (n :: Nat) (p :: k) :: [k] where
  RepeatT 0 p = GL.TypeError ('GL.Text "RepeatT is not defined for zero")
  RepeatT 1 p = p ': '[]
  RepeatT n p = p ': RepeatT (n GN.- 1) p

type family IntersperseT (s :: Symbol) (xs :: [Symbol]) :: Symbol where
  IntersperseT s '[] = ""
  IntersperseT s '[x] = x
  IntersperseT s (x ': y ': xs) = x `GL.AppendSymbol` s `GL.AppendSymbol` IntersperseT s (y ': xs)

type family LenT (xs :: [k]) :: Nat where
  LenT '[] = 0
  LenT (x ': xs) = 1 GN.+ LenT xs

-- | takes a flat n-tuple and creates a reversed inductive tuple. see 'Predicate.Prelude.PrintT'
--
-- >>> inductTupleC (123,'x',False,"abc")
-- ("abc",(False,('x',(123,()))))
--
-- >>> inductTupleC (123,'x')
-- ('x',(123,()))
--
class InductTupleC x where
  type InductTupleP x
  inductTupleC :: x -> InductTupleP x
instance (GL.TypeError ('GL.Text "InductTupleC: inductive tuple cannot be empty")) => InductTupleC () where
  type InductTupleP () = ()
  inductTupleC () = ()
instance InductTupleC (a,b) where
  type InductTupleP (a,b) = (b,(a,()))
  inductTupleC (a,b) = (b,(a,()))
instance InductTupleC (a,b,c) where
  type InductTupleP (a,b,c) = (c,(b,(a,())))
  inductTupleC (a,b,c) = (c,(b,(a,())))
instance InductTupleC (a,b,c,d) where
  type InductTupleP (a,b,c,d) = (d,(c,(b,(a,()))))
  inductTupleC (a,b,c,d) = (d,(c,(b,(a,()))))
instance InductTupleC (a,b,c,d,e) where
  type InductTupleP (a,b,c,d,e) = (e,(d,(c,(b,(a,())))))
  inductTupleC (a,b,c,d,e) = (e,(d,(c,(b,(a,())))))
instance InductTupleC (a,b,c,d,e,f) where
  type InductTupleP (a,b,c,d,e,f) = (f,(e,(d,(c,(b,(a,()))))))
  inductTupleC (a,b,c,d,e,f) = (f,(e,(d,(c,(b,(a,()))))))
instance InductTupleC (a,b,c,d,e,f,g) where
  type InductTupleP (a,b,c,d,e,f,g) = (g,(f,(e,(d,(c,(b,(a,())))))))
  inductTupleC (a,b,c,d,e,f,g) = (g,(f,(e,(d,(c,(b,(a,())))))))
instance InductTupleC (a,b,c,d,e,f,g,h) where
  type InductTupleP (a,b,c,d,e,f,g,h) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  inductTupleC (a,b,c,d,e,f,g,h) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i) where
  type InductTupleP (a,b,c,d,e,f,g,h,i) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j,k) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j,k) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j,k) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
instance InductTupleC (a,b,c,d,e,f,g,h,i,j,k,l) where
  type InductTupleP (a,b,c,d,e,f,g,h,i,j,k,l) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  inductTupleC (a,b,c,d,e,f,g,h,i,j,k,l) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))

-- | takes a list and converts to a reversed inductive tuple. see 'Predicate.Prelude.PrintL'
--
-- >>> inductListC @4 [10,12,13,1]
-- (1,(13,(12,(10,()))))
--
-- >>> inductListC @2 ["ab","cc"]
-- ("cc",("ab",()))
--
class InductListC (n :: Nat) a where
  type InductListP n a
  inductListC :: [a] -> InductListP n a
instance (GL.TypeError ('GL.Text "InductListC: inductive tuple cannot be empty")) => InductListC 0 a where
  type InductListP 0 a = ()
  inductListC _ = error "InductListC 0: shouldnt be called"
instance (GL.TypeError ('GL.Text "InductListC: inductive tuple cannot have one element")) => InductListC 1 a where
  type InductListP 1 a = a
  inductListC _ = error "InductListC 1: shouldnt be called"
instance InductListC 2 a where
  type InductListP 2 a = (a,(a,()))
  inductListC [a,b] = (b,(a,()))
  inductListC _ = error $ "inductListC: expected 2 values"
instance InductListC 3 a where
  type InductListP 3 a = (a,(a,(a,())))
  inductListC [a,b,c] = (c,(b,(a,())))
  inductListC _ = error $ "inductListC: expected 3 values"
instance InductListC 4 a where
  type InductListP 4 a = (a,(a,(a,(a,()))))
  inductListC [a,b,c,d] = (d,(c,(b,(a,()))))
  inductListC _ = error $ "inductListC: expected 4 values"
instance InductListC 5 a where
  type InductListP 5 a = (a,(a,(a,(a,(a,())))))
  inductListC [a,b,c,d,e] = (e,(d,(c,(b,(a,())))))
  inductListC _ = error $ "inductListC: expected 5 values"
instance InductListC 6 a where
  type InductListP 6 a = (a,(a,(a,(a,(a,(a,()))))))
  inductListC [a,b,c,d,e,f] = (f,(e,(d,(c,(b,(a,()))))))
  inductListC _ = error $ "inductListC: expected 6 values"
instance InductListC 7 a where
  type InductListP 7 a = (a,(a,(a,(a,(a,(a,(a,())))))))
  inductListC [a,b,c,d,e,f,g] = (g,(f,(e,(d,(c,(b,(a,())))))))
  inductListC _ = error $ "inductListC: expected 7 values"
instance InductListC 8 a where
  type InductListP 8 a = (a,(a,(a,(a,(a,(a,(a,(a,()))))))))
  inductListC [a,b,c,d,e,f,g,h] = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  inductListC _ = error $ "inductListC: expected 8 values"
instance InductListC 9 a where
  type InductListP 9 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))
  inductListC [a,b,c,d,e,f,g,h,i] = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  inductListC _ = error $ "inductListC: expected 9 values"
instance InductListC 10 a where
  type InductListP 10 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j] = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  inductListC _ = error $ "inductListC: expected 10 values"
instance InductListC 11 a where
  type InductListP 11 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k] = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  inductListC _ = error $ "inductListC: expected 11 values"
instance InductListC 12 a where
  type InductListP 12 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k,l] = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  inductListC _ = error $ "inductListC: expected 12 values"

-- partially apply the 2nd arg to an ADT -- $ and & work with functions only
-- doesnt apply more than once because we need to eval it
type family (p :: k -> k1) %% (q :: k) :: k1 where
  p %% q = p q

infixl 9 %%

type family (p :: k) %& (q :: k -> k1) :: k1 where
  p %& q = q p

infixr 9 %&

type family FlipT (d :: k1 -> k -> k2) (p :: k) (q :: k1) :: k2 where
  FlipT d p q = d q p

type family IfT (b :: Bool) (t :: k) (f :: k) :: k where
  IfT 'True t f = t
  IfT 'False t f = f

type family SumT (ns :: [Nat]) :: Nat where
  SumT '[] = 0
  SumT (n ': ns) = n GL.+ SumT ns

-- only works if you use ADTs not type synonyms
type family MapT (f :: k -> k1) (xs :: [k]) :: [k1] where
  MapT f '[] = '[]
  MapT f (x ': xs) = f x ': MapT f xs

-- | Extract \'a\' from a list like container
type family ConsT s where
  ConsT [a] = a
  ConsT (ZipList a) = a
  ConsT T.Text = Char
  ConsT ByteString = Word8
  ConsT (Seq a) = a
  ConsT s  = GL.TypeError (
      'GL.Text "invalid ConsT instance"
      ':$$: 'GL.Text "s = "
      ':<>: 'GL.ShowType s)

-- | used by "Predicate.Refined3" for extracting \'ip\' from a 4-tuple
type family T_1 x where
  T_1 '(a,b,c,d) = a
  T_1 o = GL.TypeError (
      'GL.Text "invalid T_1 instance"
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | used by "Predicate.Refined3" for extracting the boolean predicate \'op\' from a 4-tuple
type family T_2 x where
  T_2 '(a,b,c,d) = b
  T_2 o = GL.TypeError (
      'GL.Text "invalid T_2 instance"
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | used by "Predicate.Refined3" for extracting \'fmt\' from a 4-tuple
type family T_3 x where
  T_3 '(a,b,c,d) = c
  T_3 o = GL.TypeError (
      'GL.Text "invalid T_3 instance"
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | used by "Predicate.Refined3" for extracting the input type \'i\' from a 4-tuple
type family T_4 x where
  T_4 '(a,b,c,d) = d
  T_4 o = GL.TypeError (
      'GL.Text "invalid T_4 instance"
      ':$$: 'GL.Text "o = "
      ':<>: 'GL.ShowType o)

-- | a typeclass for choosing which monad to run in
class Monad m => MonadEval m where
  runIO :: IO a -> m (Maybe a)
  catchit :: E.Exception e => a -> m (Either String a)
  catchitNF :: (E.Exception e, NFData a) => a -> m (Either String a)
  liftEval :: m a -> IO a

instance MonadEval Identity where
  runIO _ = Identity Nothing
  catchit v = Identity $ unsafePerformIO $ catchit @IO @E.SomeException v
  catchitNF v = Identity $ unsafePerformIO $ catchitNF @IO @E.SomeException v
  liftEval = return . runIdentity

instance MonadEval IO where
  runIO ioa = Just <$> ioa
  catchit v = E.evaluate (Right $! v) `E.catch` (\(E.SomeException e) -> pure $ Left ("IO e=" <> show e))
  catchitNF v = E.evaluate (Right $!! v) `E.catch` (\(E.SomeException e) -> pure $ Left ("IO e=" <> show e))
  liftEval = id

-- | strip ansi characters from a string
removeAnsi :: Show a => Either String a -> IO ()
removeAnsi =
  \case
     Left e -> let esc = '\x1b'
                   f :: String -> Maybe (String, String)
                   f = \case
                          [] -> Nothing
                          c:cs | c == esc -> case break (=='m') cs of
                                                  (_,'m':s) -> Just ("",s)
                                                  _ -> Nothing
                               | otherwise -> Just $ break (==esc) (c:cs)
               in putStrLn $ concat $ unfoldr f e
     Right a -> print a

