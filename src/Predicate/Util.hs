{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     Utility methods for Predicate / methods for displaying the evaluation tree
-}
module Predicate.Util (
  -- ** TT
    TT(..)
  , tBool
  , tString
  , tForest
  , fixBoolT
  , topMessage
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
  , pString
  , pBool

 -- ** create tree functions
  , mkNode
  , mkNodeB
  , mkNodeSkipP

 -- ** tree manipulation
  , getValAndPE
  , getValLRFromTT
  , fromTT
  , getValueLR
  , getValueLRHide
  , fixLite
  , fixit
  , prefixMsg
  , splitAndAlign

 -- ** display options
  , POptsL
  , POpts
  , ODebug(..)
  , Disp(..)
  , defOpts
  , PColor(..)
  , nocolor
  , color1
  , color2
  , color3
  , color4
  , color5
  , colorMe
  , isVerbose
  , showBoolP
  , reifyOpts
  , setWidth
  , setUnicode
  , setAnsi
  , setColor
  , setColorImpl
  , setDebug
  , HOpts (..)
  , OptT(..)
  , OptTC(..)
  , getOptT

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
  , showL
  , litL
  , litBL
  , litBS

  -- ** regular expressions
  , ROpt(..)
  , compileRegex
  , GetROpts(..)
  , RReplace(..)
  , ReplaceFnSubC(..)
  , ReplaceFnSub(..)

  -- ** useful type families
  , ZwischenT
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
  , type (<%>)

 -- ** extract values from the type level
  , nat
  , symb
  , GetNats(..)
  , GetSymbs(..)
  , GetLen(..)
  , GetThese(..)
  , GetOrdering(..)
  , GetBool(..)
  , OrderingP(..)
  , GetOrd(..)

 -- ** printing methods
  , prtTTIO
  , prtTT
  , prtTree
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
  , errorInProgram
  , readField
  , showThese

  -- ** extract from n-tuple
  , T4_1
  , T4_2
  , T4_3
  , T4_4
  , T5_1
  , T5_2
  , T5_3
  , T5_4
  , T5_5

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
--import Data.Char
import Data.Data
import System.Console.Pretty
import GHC.Exts (Constraint)
import qualified Text.Regex.PCRE.Heavy as RH
import qualified Text.Regex.PCRE.Light as RL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative (ZipList)
import Data.Kind (Type)
import Data.These (These(..))
import qualified Control.Exception as E
import Control.DeepSeq
import System.IO.Unsafe (unsafePerformIO)
import Data.Bool
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Either
import qualified Text.Read.Lex as L
import Text.ParserCombinators.ReadPrec
import qualified GHC.Read as GR
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS8
import GHC.Stack
import Data.Monoid (Last (..))
import Data.Maybe

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | represents the evaluation tree for predicates
data TT a = TT { _tBool :: !(BoolT a)  -- ^ the value at this root node
               , _tString :: !String  -- ^ detailed information eg input and output and text
               , _tForest :: !(Forest PE) -- ^ the child nodes
               } deriving Show

-- | contains the typed result from evaluating the expression tree
--
data BoolT a where
  FailT :: !String -> BoolT a  -- failure with string
  FalseT :: BoolT Bool        -- false predicate
  TrueT :: BoolT Bool         -- true predicate
  PresentT :: !a -> BoolT a    -- non predicate value
{- too restrictive
instance Semigroup a => Semigroup (BoolT a) where
   PresentT a <> PresentT a1 = PresentT (a <> a1)
-}

-- | semigroup instance for 'BoolT'
--
instance Semigroup (BoolT a) where
   FailT s <> FailT s1 = FailT (s <> s1)
   FailT s <> _ = FailT s
   _ <> FailT s = FailT s
   FalseT <> _ = FalseT
   _ <> FalseT = FalseT
   TrueT <> TrueT = TrueT
   TrueT <> PresentT a = PresentT a
   PresentT a <> TrueT = PresentT a
   PresentT a <> PresentT _ = PresentT a


deriving instance Show a => Show (BoolT a)
deriving instance Eq a => Eq (BoolT a)

-- | lens for accessing 'BoolT' in 'TT'
tBool :: Lens (TT a) (TT b) (BoolT a) (BoolT b)
tBool afb s = (\b -> s { _tBool = b }) <$> afb (_tBool s)

tString :: Lens' (TT a) String
tString afb s = (\b -> s { _tString = b }) <$> afb (_tString s)

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
    FailP !String -- ^ fails the entire evaluation
  | FalseP       -- ^ False predicate
  | TrueP        -- ^ True predicate
  | PresentP     -- ^ Any value
  deriving (Show, Eq)

-- | represents the untyped evaluation tree for final display
data PE = PE { _pBool :: !BoolP -- ^ holds the result of running the predicate
             , _pString :: !String -- ^ optional strings to include in the results
             } deriving Show

pBool :: Lens' PE BoolP
pBool afb (PE x y) = flip PE y <$> afb x

pString :: Lens' PE String
pString afb s = (\b -> s { _pString = b }) <$> afb (_pString s)

-- | creates a Node for the evaluation tree
mkNode :: POpts -> BoolT a -> String -> [Holder] -> TT a
mkNode opts bt ss hs =
  case oDebug opts of
    OZero -> TT bt [] []
    OLite -> TT bt ss [] -- keeps the last one so we can use the root to give more details on failure (especially for Refined* types)
    _ -> TT bt ss (map fromTTH hs)

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts -> Bool -> String -> [Holder] -> TT Bool
mkNodeB opts b = mkNode opts (bool FalseT TrueT b)

mkNodeSkipP :: Tree PE
mkNodeSkipP = Node (PE TrueP "skipped PP ip i = Id") []

getValAndPE :: TT a -> (Either String a, Tree PE)
getValAndPE tt = (getValLRFromTT tt, fromTT tt)

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
data Holder = forall w . Holder !(TT w)

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

-- elide FailT msg in tString[0] if showError is False
-- | a helper method to add extra context on failure to the tree or extract the value at the root of the tree
getValueLRImpl :: Bool -> POpts -> String -> TT a -> [Holder] -> Either (TT x) a
getValueLRImpl showError opts msg0 tt hs =
  let tt' = hs ++ [hh tt]
  in left (\e -> mkNode
                   opts
                  (FailT e)
                   (msg0 <> if showError || isVerbose opts then (if null msg0 then "" else " ") <> "[" <> e <> "]"
                            else "")
                  tt'
          )
          (getValLRFromTT tt)

-- | the color palette for displaying the expression tree
newtype PColor = PColor (BoolP -> String -> String)
instance Show PColor where
  show PColor {} = "PColor <fn>"

type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

type POpts = HOpts Identity

-- | customizable options
data HOpts f =
  HOpts { oWidth :: !(HKD f Int) -- ^ length of data to display for 'showLitImpl'
        , oDebug :: !(HKD f ODebug) -- ^ debug level
        , oDisp :: !(HKD f Disp) -- ^ display the tree using the normal tree or unicode
        , oColor :: !(HKD f (String, PColor)) -- ^ color palette used
        }

deriving instance
  ( Show (HKD f Int)
  , Show (HKD f ODebug)
  , Show (HKD f Disp)
  , Show (HKD f (String, PColor))
  ) => Show (HOpts f)

reifyOpts :: HOpts Last -> HOpts Identity
reifyOpts h =
  HOpts (fromMaybe (oWidth defOpts) (getLast (oWidth h)))
           (fromMaybe (oDebug defOpts) (getLast (oDebug h)))
           (fromMaybe (oDisp defOpts) (getLast (oDisp h)))
           (fromMaybe (oColor defOpts) (getLast (oColor h)))

setWidth :: Int -> POptsL
setWidth i = mempty { oWidth = pure i }

setDebug :: Int -> POptsL
setDebug i =
  let (mn,mx) = (fromEnum (minBound :: ODebug), fromEnum (maxBound :: ODebug))
      ret = toEnum (min (max i mn) mx)
  in mempty { oDebug = pure ret }

setAnsi :: POptsL
setAnsi = mempty { oDisp = pure Ansi }

setUnicode :: POptsL
setUnicode = mempty { oDisp = pure Unicode }

setColorImpl :: (String, PColor) -> POptsL
setColorImpl c = mempty { oColor = pure c }

setColor :: Int -> POptsL
setColor i =
  let ret=
       if | i == 0 -> nocolor
          | i == 1 -> color1
          | i == 2 -> color2
          | i == 3 -> color3
          | i == 4 -> color4
          | i == 5 -> color5
          | otherwise -> error "dude: invalid color"
  in mempty { oColor = pure ret }

type POptsL = HOpts Last

instance Monoid (HOpts Last) where
  mempty = HOpts mempty mempty mempty mempty

instance Semigroup (HOpts Last) where
  HOpts a b c d <> HOpts a' b' c' d' = HOpts (a <> a') (b <> b') (c <> c') (d <> d')

--seqPOptsM :: HOpts Last -> Maybe (HOpts Identity)
--seqPOptsM h = coerce (HOpts <$> oWidth h <*> oDebug h <*> oDisp h <*> oColor h)

-- | display format for the tree
data Disp = Ansi -- ^ draw normal tree
          | Unicode  -- ^ use unicode
          deriving (Show, Eq)

defOpts :: POpts
defOpts = HOpts
    { oWidth = 200
    , oDebug = ONormal
    , oDisp = Ansi
    , oColor = color5
    }

data ODebug =
       OZero -- ^ one line summary used mainly for testing
     | OLite -- ^ one line summary with additional context from the head of the evaluation tree
     | OSubNormal -- ^ outputs the evaluation tree but skips noisy subtrees
     | ONormal  -- ^ outputs the evaluation tree but skips noisy subtrees
     | OVerbose -- ^ outputs the entire evaluation tree
     deriving (Ord, Show, Eq, Enum, Bounded)

-- | helper method to set the debug level
isVerbose :: POpts -> Bool
isVerbose = (OVerbose==) . oDebug

-- | color palettes
--
-- italics dont work but underline does
nocolor, color1, color2, color3, color4, color5 :: (String, PColor)

-- | no colors are displayed
nocolor = ("nocolor", PColor $ flip const)

-- | default color palette
color1 =
  ("color1",) $ PColor $ \case
    FailP {} -> bgColor Blue
    FalseP -> bgColor Red
    TrueP -> colorize Foreground Black . bgColor Cyan
    PresentP -> colorize Foreground Black . bgColor Yellow

color2 =
  ("color2",) $ PColor $ \case
    FailP {} -> bgColor Magenta
    FalseP -> bgColor Red
    TrueP -> colorize Foreground Black . bgColor White
    PresentP -> colorize Foreground Black . bgColor Yellow

color3 =
  ("color3",) $ PColor $ \case
    FailP {} -> bgColor Blue
    FalseP -> color Red
    TrueP -> color White
    PresentP -> colorize Foreground Black . bgColor Yellow

color4 =
  ("color4",) $ PColor $ \case
    FailP {} -> bgColor Red
    FalseP -> color Red
    TrueP -> color Green
    PresentP -> colorize Foreground Black . bgColor Yellow

color5 =
  ("color5",) $ PColor $ \case
    FailP {} -> color Blue
    FalseP -> color Red
    TrueP -> color Cyan
    PresentP -> color Yellow

-- | fix PresentT Bool to TrueT or FalseT
fixBoolT :: TT Bool -> TT Bool
fixBoolT t =
  case t ^? tBool . _PresentT of
    Nothing -> t
    Just b -> t & tBool .~ _boolT # b

show01 :: (Show a1, Show a2) => POpts -> String -> a1 -> a2 -> String
show01 opts msg0 ret = lit01 opts msg0 ret . show

lit01 :: Show a1 => POpts -> String -> a1 -> String -> String
lit01 opts msg0 ret as = lit01' opts msg0 ret "" as

show01' :: (Show a1, Show a2) => POpts -> String -> a1 -> String -> a2 -> String
show01' opts msg0 ret fmt = lit01' opts msg0 ret fmt . show

lit01' :: Show a1 => POpts -> String -> a1 -> String -> String -> String
lit01' opts msg0 ret fmt as
  | null fmt && null as = msg0
  | otherwise =
         msg0
      <> show0 opts " " ret
      <> showLit1 opts (" | " ++ fmt) as

-- | display all data regardless of debug level
showLit0 :: POpts -> String -> String -> String
showLit0 o = showLitImpl o OLite

-- | more restrictive: only display data at debug level 1 or less
showLit1 :: POpts -> String -> String -> String
showLit1 o = showLitImpl o OLite

showLitImpl :: POpts -> ODebug -> String -> String -> String
showLitImpl o i s a =
  if oDebug o >= i then s <> litL (oWidth o) a
  else ""

show0 :: Show a => POpts -> String -> a -> String
show0 o = showAImpl o OLite

show3 :: Show a => POpts -> String -> a -> String
show3 o = showAImpl o OVerbose

show1 :: Show a => POpts -> String -> a -> String
show1 o = showAImpl o OLite

showAImpl :: Show a => POpts -> ODebug -> String -> a -> String
showAImpl o i s a = showLitImpl o i s (show a)

showL :: Show a => Int -> a -> String
showL i = litL i . show

litL :: Int -> String -> String
litL i s = take i s <> if length s > i then "..." else ""

litBL :: Int -> BL8.ByteString -> String
litBL i s = litL i (BL8.unpack (BL8.take (fromIntegral i+1) s))

litBS :: Int -> BS8.ByteString -> String
litBS i s = litL i (BS8.unpack (BS8.take (i+1) s))

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
compileRegex opts nm s hhs
  | null s = Left (mkNode opts (FailT "Regex cannot be empty") nm hhs)
  | otherwise =
      let rs = getROpts @rs
          mm = nm <> " " <> show rs
      in flip left (RH.compileM (TE.encodeUtf8 (T.pack s)) rs)
            $ \e -> mkNode opts (FailT "Regex failed to compile") (mm <> " compile failed with regex msg[" <> e <> "]") hhs

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

data ReplaceFnSub = RPrepend | ROverWrite | RAppend deriving (Show,Eq)

class ReplaceFnSubC (k :: ReplaceFnSub) where
  getReplaceFnSub :: ReplaceFnSub
instance ReplaceFnSubC 'RPrepend where getReplaceFnSub = RPrepend
instance ReplaceFnSubC 'ROverWrite where getReplaceFnSub = ROverWrite
instance ReplaceFnSubC 'RAppend where getReplaceFnSub = RAppend

-- | used by 'Predicate.ReplaceImpl' and 'RH.sub' and 'RH.gsub' to allow more flexible replacement
--   These parallel the RegexReplacement (not exported) class in "Text.Regex.PCRE.Heavy" but have overlappable instances which is problematic for this code so I use 'RReplace'
data RReplace =
     RReplace !ReplaceFnSub !String
   | RReplace1 !(String -> [String] -> String)
   | RReplace2 !(String -> String)
   | RReplace3 !([String] -> String)

instance Show RReplace where
  show = \case
           RReplace o s -> "RReplace " ++ show o ++ " " ++ s
           RReplace1 {} -> "RReplace1 <fn>"
           RReplace2 {} -> "RReplace2 <fn>"
           RReplace3 {} -> "RReplace3 <fn>"

-- | extract values from the trees or if there are errors returned a tree with added context
splitAndAlign :: Show x =>
                    POpts
                    -> String
                    -> [((Int, x), TT a)]
                    -> Either (TT w) [(a, (Int, x), TT a)]
splitAndAlign opts msgs ts =
  case partitionEithers (map partitionTTExtended ts) of
     (excs@(e:_), _) ->
          Left $ mkNode opts
                       (FailT (groupErrors (map snd excs)))
                       (msgs <> (formatList opts [fst e] <> " excnt=" <> show (length excs)))
                       (map (hh . snd) ts)
     ([], tfs) -> Right tfs

partitionTTExtended :: (w, TT a) -> Either ((w, TT x), String) (a, w, TT a)
partitionTTExtended (s, t) =
  case _tBool t of
    FailT e -> Left ((s, t & tBool .~ FailT e), e)
    PresentT a -> Right (a,s,t)
    TrueT -> Right (True,s,t)
    FalseT -> Right (False,s,t)

formatList :: forall x z . Show x => POpts -> [((Int, x), z)] -> String
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showAImpl opts OLite ", a=" a <> ")")

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
infixr 1 ~>

-- | type level Between
type family ZwischenT (a :: Nat) (b :: Nat) (v :: Nat) :: Constraint where
  ZwischenT m n v =
     FailUnlessT (AndT (m GL.<=? v) (v GL.<=? n))
            ('GL.Text "ZwischenT failure"
             ':$$: 'GL.ShowType v
             ':$$: 'GL.Text " is outside of "
             ':$$: 'GL.ShowType m
             ':<>: 'GL.Text " and "
             ':<>: 'GL.ShowType n)

-- | helper method to fail with a msg when True
type family FailWhenT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailWhenT 'False _ = ()
  FailWhenT 'True e = GL.TypeError e

-- | helper method to fail with msg when False
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

-- | get the length of a typelevel container
--
-- >>> getLen @'["abc","def","g"]
-- 3
--
-- >>> getLen @'[]
-- 0
--
-- >>> getLen @(9 ':| '[1,2,3])
-- 4
--
-- >>> getLen @('These 9 "Asfs")
-- 1
--
-- >>> getLen @('This 1)
-- 0
--
class GetLen xs where -- (xs :: [k]) will break it! ghc 8.6.5
  getLen :: Int
instance GetLen '[] where
  getLen = 0
instance GetLen xs => GetLen (x ': xs) where
  getLen = 1 + getLen @xs
instance GetLen ('Just a) where
  getLen = 1
instance GetLen 'Nothing where
  getLen = 0
instance GetLen ('Left a) where
  getLen = 0
instance GetLen ('Right a) where
  getLen = 1
instance GetLen ('This a) where
  getLen = 0
instance GetLen ('That a) where
  getLen = 1
instance GetLen ('These a b) where
  getLen = 1
instance GetLen xs => GetLen (x ':| xs) where
  getLen = 1 + getLen @xs


showThese :: These a b -> String
showThese = \case
  This {} -> "This"
  That {} -> "That"
  These {} -> "These"

class GetThese th where
  getThese :: (String, These w v -> Bool)
instance GetThese ('This x) where
  getThese = ("This", isThis)
instance GetThese ('That y) where
  getThese = ("That", isThat)
instance GetThese ('These x y) where
  getThese = ("These", isThese)

isThis :: These a b -> Bool
isThis This {} = True
isThis _ = False

isThat :: These a b -> Bool
isThat That {} = True
isThat _ = False

isThese :: These a b -> Bool
isThese These {} = True
isThese _ = False

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

data OrderingP = CGt | CGe | CEq | CLe | CLt | CNe deriving (Show, Eq, Enum, Bounded)

class GetOrd (k :: OrderingP) where
  getOrd :: Ord a => (String, a -> a -> Bool)

instance GetOrd 'CGt where getOrd = (">", (>))
instance GetOrd 'CGe where getOrd = (">=",(>=))
instance GetOrd 'CEq where getOrd = ("==",(==))
instance GetOrd 'CLe where getOrd = ("<=",(<=))
instance GetOrd 'CLt where getOrd = ("<", (<))
instance GetOrd 'CNe where getOrd = ("/=",(/=))

toNodeString :: POpts -> PE -> String
toNodeString opts bpe =
  if hasNoTree opts then errorInProgram $ "shouldnt be calling this if we are dropping details: toNodeString " <> show (oDebug opts) <> " " <> show bpe
  else showBoolP opts (_pBool bpe) <> " " <> _pString bpe

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
    b@TrueP -> colorMe o b "True"
    b@FalseP -> colorMe o b "False"
{-
displayMessages :: [String] -> String
displayMessages es =
  case filter (not . all isSpace) es of
    [] -> ""
    z -> intercalate " | " z
-}
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
prtTree o = putStr . prtTreePure o

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

topMessage :: TT a -> String
topMessage pp =  "(" <> (pp ^. tString) <> ")"

showImpl :: POpts -> Tree String -> String
showImpl o =
  case oDisp o of
    Unicode -> TV.showTree
    Ansi -> drawTree -- to drop the last newline else we have to make sure that everywhere else has that newline: eg fixLite

prettyRational :: Rational -> String
prettyRational (numerator &&& denominator -> (n,d)) =
  if | n == 0 -> "0"
     | d == 1 -> show n
     | otherwise -> show n <> " / " <> show d

fixit :: ((Int, x), TT a) -> TT a
fixit ((i, _), t) = prefixMsg ("i=" <> show i <> ":") t

prefixMsg :: String -> TT a -> TT a
prefixMsg msg t =
   t & tString %~ (msg <>)

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

type s <%> t = GL.AppendSymbol s t
infixr 7 <%>

type family IntersperseT (s :: Symbol) (xs :: [Symbol]) :: Symbol where
  IntersperseT s '[] = ""
  IntersperseT s '[x] = x
  IntersperseT s (x ': y ': xs) = x <%> s <%> IntersperseT s (y ': xs)

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
  inductListC _ = errorInProgram "InductListC 0: shouldnt be called"
instance (GL.TypeError ('GL.Text "InductListC: inductive tuple cannot have one element")) => InductListC 1 a where
  type InductListP 1 a = a
  inductListC _ = errorInProgram "InductListC 1: shouldnt be called"
instance InductListC 2 a where
  type InductListP 2 a = (a,(a,()))
  inductListC [a,b] = (b,(a,()))
  inductListC _ = errorInProgram "inductListC: expected 2 values"
instance InductListC 3 a where
  type InductListP 3 a = (a,(a,(a,())))
  inductListC [a,b,c] = (c,(b,(a,())))
  inductListC _ = errorInProgram "inductListC: expected 3 values"
instance InductListC 4 a where
  type InductListP 4 a = (a,(a,(a,(a,()))))
  inductListC [a,b,c,d] = (d,(c,(b,(a,()))))
  inductListC _ = errorInProgram "inductListC: expected 4 values"
instance InductListC 5 a where
  type InductListP 5 a = (a,(a,(a,(a,(a,())))))
  inductListC [a,b,c,d,e] = (e,(d,(c,(b,(a,())))))
  inductListC _ = errorInProgram "inductListC: expected 5 values"
instance InductListC 6 a where
  type InductListP 6 a = (a,(a,(a,(a,(a,(a,()))))))
  inductListC [a,b,c,d,e,f] = (f,(e,(d,(c,(b,(a,()))))))
  inductListC _ = errorInProgram "inductListC: expected 6 values"
instance InductListC 7 a where
  type InductListP 7 a = (a,(a,(a,(a,(a,(a,(a,())))))))
  inductListC [a,b,c,d,e,f,g] = (g,(f,(e,(d,(c,(b,(a,())))))))
  inductListC _ = errorInProgram "inductListC: expected 7 values"
instance InductListC 8 a where
  type InductListP 8 a = (a,(a,(a,(a,(a,(a,(a,(a,()))))))))
  inductListC [a,b,c,d,e,f,g,h] = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  inductListC _ = errorInProgram "inductListC: expected 8 values"
instance InductListC 9 a where
  type InductListP 9 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))
  inductListC [a,b,c,d,e,f,g,h,i] = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  inductListC _ = errorInProgram "inductListC: expected 9 values"
instance InductListC 10 a where
  type InductListP 10 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j] = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  inductListC _ = errorInProgram "inductListC: expected 10 values"
instance InductListC 11 a where
  type InductListP 11 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,())))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k] = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  inductListC _ = errorInProgram "inductListC: expected 11 values"
instance InductListC 12 a where
  type InductListP 12 a = (a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,(a,()))))))))))))
  inductListC [a,b,c,d,e,f,g,h,i,j,k,l] = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  inductListC _ = errorInProgram "inductListC: expected 12 values"

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
  -- IfT b x x = x -- todo: benefit? now it needs to eval both sides
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

-- | strip ansi characters from a string and print it (for doctests)
removeAnsi :: Show a => Either String a -> IO ()
removeAnsi = putStrLn . removeAnsiImpl

removeAnsiImpl :: Show a => Either String a -> String
removeAnsiImpl =
  \case
     Left e -> let esc = '\x1b'
                   f :: String -> Maybe (String, String)
                   f = \case
                          [] -> Nothing
                          c:cs | c == esc -> case break (=='m') cs of
                                                  (_,'m':s) -> Just ("",s)
                                                  _ -> Nothing
                               | otherwise -> Just $ break (==esc) (c:cs)
               in concat $ unfoldr f e
     Right a -> show a

errorInProgram :: HasCallStack => String -> x
errorInProgram s = error $ "programmer error:" <> s

readField :: String -> ReadPrec a -> ReadPrec a
readField fieldName readVal = do
        GR.expectP (L.Ident fieldName)
        GR.expectP (L.Punc "=")
        readVal

data OptT = OZ | OL | OAN | OA | OAB | OU | OUB | OC !Nat | OD !Nat | OW !Nat | OEmpty | !OptT :*: !OptT

instance Show OptT where
  show = \case
            OZ -> "OZ"
            OL -> "OL"
            OAN -> "OAN"
            OA -> "OA"
            OAB -> "OAB"
            OU -> "OU"
            OUB -> "OUB"
            OC _n -> "OC"
            OD _n -> "OD"
            OW _n -> "OW"
            OEmpty -> "OEmpty"
            a :*: b -> show a ++ " ':*: " ++ show b
infixr 6 :*:
class OptTC (k :: OptT) where getOptT' :: POptsL
instance OptTC 'OZ where getOptT' = setAnsi <> setColor 0 <> setDebug 0
instance OptTC 'OL where getOptT' = setAnsi <> setColor 0 <> setDebug 1
instance OptTC 'OAN where getOptT' = setAnsi <> setColor 0
instance OptTC 'OA where getOptT' = setAnsi <> setColor 5
instance OptTC 'OAB where getOptT' = setAnsi <> setColor 1
instance OptTC 'OU where getOptT' = setUnicode <> setColor 5
instance OptTC 'OUB where getOptT' = setUnicode <> setColor 1
instance KnownNat n => OptTC ('OC n) where getOptT' = setColor (nat @n)
instance KnownNat n => OptTC ('OD n) where getOptT' = setDebug (nat @n)
instance KnownNat n => OptTC ('OW n) where getOptT' = setWidth (nat @n)
instance OptTC 'OEmpty where getOptT' = mempty
instance (OptTC a, OptTC b) => OptTC (a ':*: b) where getOptT' = getOptT' @a <> getOptT' @b

-- | convert typelevel options to 'POpts'
--
-- >>> getOptT @('OA ':*: 'OC 3 ':*: 'OU  ':*: 'OA ':*: 'OW 321)
-- HOpts {oWidth = 321, oDebug = ONormal, oDisp = Ansi, oColor = ("color5",PColor <fn>)}
--
getOptT :: forall o . OptTC o => POpts
getOptT = reifyOpts (getOptT' @o)

type family T4_1 x where
  T4_1 '(a,_,_,_) = a
type family T4_2 x where
  T4_2 '(_,b,_,_) = b
type family T4_3 x where
  T4_3 '(_,_,c,_) = c
type family T4_4 x where
  T4_4 '(_,_,_,d) = d

type family T5_1 x where
  T5_1 '(a,_,_,_,_) = a
type family T5_2 x where
  T5_2 '(_,b,_,_,_) = b
type family T5_3 x where
  T5_3 '(_,_,c,_,_) = c
type family T5_4 x where
  T5_4 '(_,_,_,d,_) = d
type family T5_5 x where
  T5_5 '(_,_,_,_,e) = e
