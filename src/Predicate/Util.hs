{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  , GetBoolT(..)
  , _FailT
  , _PresentT
  , _FalseT
  , _TrueT

 -- ** PE
  , PE
  , pString

 -- ** create tree functions
  , mkNode
  , mkNodeB
  , mkNodeSkipP

 -- ** tree manipulation
  , getValAndPE
  , getValLRFromTT
  , fromTT
  , getValueLR
  , fixLite
  , fixit
  , prefixMsg
  , splitAndAlign

 -- ** display options
  , POptsL
  , POpts
  , Debug(..)
  , Disp(..)
  , Color(..)
  , isVerbose
  , colorBoolT
  , colorBoolT'
  , setOtherEffects
  , type Color1
  , type Color2
  , type Color3
  , type Color4
  , type Color5
  , type Other1
  , type Other2
  , type OZ
  , type OL
  , type OAN
  , type OANV
  , type OA
  , type OAB
  , type OU
  , type OUB
  , type OUV
  , type OAV

  , HOpts(..)
  , OptT(..)
  , OptTC(..)
  , type OptTT
  , getOptT
  , subopts

-- ** formatting functions
  , show01
  , show01'
  , lit01
  , litVerbose
  , showVerbose
  , showL
  , litL
  , litBL
  , litBS

  -- ** regular expressions
  , ROpt(..)
  , compileRegex
  , GetROpts(..)
  , RReplace(..)
  , GetReplaceFnSub(..)
  , ReplaceFnSub(..)
  , displayROpts

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
  , AnyT

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
  , prtTreePure
  , prettyRational
  , formatOMsg

 -- ** boolean methods
  , (~>)

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

 -- ** miscellaneous
  , Holder
  , hh
  , showT
  , showTK
  , prettyOrd
  , removeAnsi
  , MonadEval(..)
  , errorInProgram
  , readField
  , showThese
  , chkSize
  , pureTryTest
  , pureTryTestPred
  , isPrime

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
import Data.These.Combinators (isThis, isThat, isThese)
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
import Data.Coerce
import Data.Foldable (toList)

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
data BoolT a where
  FailT :: !String -> BoolT a  -- failure with string
  FalseT :: BoolT Bool        -- false predicate
  TrueT :: BoolT Bool         -- true predicate
  PresentT :: !a -> BoolT a    -- non predicate value

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
   PresentT _ <> PresentT a = PresentT a

deriving instance Show a => Show (BoolT a)
deriving instance Eq a => Eq (BoolT a)

-- | extracts the 'BoolT a' constructors from the typelevel
class GetBoolT a (x :: BoolT a) | x -> a where
  getBoolT :: Either Bool Bool
instance GetBoolT Bool 'TrueT where
  getBoolT = Left True
instance GetBoolT Bool 'FalseT where
  getBoolT = Left False
instance GetBoolT a ('PresentT b) where
  getBoolT = Right True
instance GetBoolT a ('FailT s) where
  getBoolT = Right False

-- | lens for accessing 'BoolT' in 'TT'
tBool :: Lens (TT a) (TT b) (BoolT a) (BoolT b)
tBool afb s = (\b -> s { _tBool = b }) <$> afb (_tBool s)

-- | lens for accessing the message from 'BoolT'
tString :: Lens' (TT a) String
tString afb s = (\b -> s { _tString = b }) <$> afb (_tString s)

-- | lens for accessing the subtree from 'BoolT'
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

-- | lens for accessing '_pBool'
pBool :: Lens' PE BoolP
pBool afb s = (\b -> s { _pBool = b }) <$> afb (_pBool s)

-- | lens for accessing 'PE'
pString :: Lens' PE String
pString afb s = (\b -> s { _pString = b }) <$> afb (_pString s)

-- | creates a Node for the evaluation tree
mkNode :: POpts
       -> BoolT a
       -> String
       -> [Holder]
       -> TT a
mkNode opts bt ss hs =
  case oDebug opts of
    DZero -> TT bt [] []
    DLite -> TT bt ss [] -- keeps the last one so we can use the root to give more details on failure (especially for Refined* types)
    _ -> TT bt ss (map fromTTH hs)

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts
        -> Bool
        -> String
        -> [Holder]
        -> TT Bool
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
getValueLR :: POpts
           -> String
           -> TT a
           -> [Holder]
           -> Either (TT x) a
getValueLR opts msg0 tt hs =
  let tt' = hs ++ [hh tt]
  in left (\e -> mkNode
                   opts
                  (FailT e)
                   msg0
                  tt'
          )
          (getValLRFromTT tt)

-- | wrapper for a show instance around 'Color'
newtype SColor = SColor Color

instance Show SColor where
  show (SColor c) =
    case c of
      Black-> "Black"
      Red-> "Red"
      Green-> "Green"
      Yellow-> "Yellow"
      Blue-> "Blue"
      Magenta-> "Magenta"
      Cyan-> "Cyan"
      White-> "White"
      Default -> "Default"

-- | the color palette for displaying the expression tree
newtype PColor = PColor (BoolP -> String -> String)
instance Show PColor where
  show PColor {} = "PColor <fn>"

-- | elide the 'Identity' wrapper so it acts like a normal adt
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

type POpts = HOpts Identity

-- | customizable options for running a typelevel expression
data HOpts f =
  HOpts { oWidth :: !(HKD f Int) -- ^ length of data to display for 'showLitImpl'
        , oDebug :: !(HKD f Debug) -- ^ debug level
        , oDisp :: !(HKD f Disp) -- ^ display the tree using the normal tree or unicode
        , oColor :: !(HKD f (String, PColor)) -- ^ color palette used
        , oMsg :: ![String] -- ^ messages associated with type
        , oRecursion :: !(HKD f Int) -- ^ max recursion
        , oOther :: !(HKD f (Bool, SColor, SColor)) -- ^ other message effects
        , oNoColor :: !(HKD f Bool) -- ^ no colors
        }

deriving instance
  ( Show (HKD f Int)
  , Show (HKD f Debug)
  , Show (HKD f Disp)
  , Show (HKD f (String, PColor))
  , Show (HKD f Bool)
  , Show (HKD f (Bool, SColor, SColor))
  ) => Show (HOpts f)

-- | combine options ala monoid
reifyOpts :: HOpts Last -> HOpts Identity
reifyOpts h =
  HOpts (fromMaybe (oWidth defOpts) (getLast (oWidth h)))
        (fromMaybe (oDebug defOpts) (getLast (oDebug h)))
        (fromMaybe (oDisp defOpts) (getLast (oDisp h)))
        (if fromMaybe (oNoColor defOpts) (getLast (oNoColor h)) then nocolor
         else fromMaybe (oColor defOpts) (getLast (oColor h)))
        (oMsg defOpts <> oMsg h)
        (fromMaybe (oRecursion defOpts) (getLast (oRecursion h)))
        (if fromMaybe (oNoColor defOpts) (getLast (oNoColor h)) then otherDef
         else fromMaybe (oOther defOpts) (getLast (oOther h)))
        (fromMaybe (oNoColor defOpts) (getLast (oNoColor h)))

-- | set maximum display width of expressions
setWidth :: Int -> POptsL
setWidth i = mempty { oWidth = pure i }

-- | set title message for the display tree
setMessage :: String -> POptsL
setMessage s = mempty { oMsg = pure s }

-- | set maximum recursion eg when running regex
setRecursion :: Int -> POptsL
setRecursion i = mempty { oRecursion = pure i }

-- | set color of title message
setOther :: Bool
         -> Color
         -> Color
         -> POptsL
setOther b c1 c2 = mempty { oOther = pure $ coerce (b, c1, c2) }

-- | turn on/off colors
setNoColor :: Bool -> POptsL
setNoColor b = mempty { oNoColor = pure b }

-- | display type eg 'Unicode' or 'Ansi'
setDisp :: Disp -> POptsL
setDisp d = mempty { oDisp = pure d }

-- | create color palette for the expression tree
setCreateColor :: String
   -> Color
   -> Color
   -> Color
   -> Color
   -> Color
   -> Color
   -> Color
   -> Color
   -> POptsL
setCreateColor s c1 c2 c3 c4 c5 c6 c7 c8 =
  let pc = \case
       FailP {} -> color c1 . bgColor c2
       FalseP -> color c3 . bgColor c4
       TrueP -> color c5 . bgColor c6
       PresentP -> color c7 . bgColor c8
  in mempty { oColor = pure (s,PColor pc) }

-- | set debug mode
setDebug :: Debug -> POptsL
setDebug d =
  mempty { oDebug = pure d }

-- | monoid opts
type POptsL = HOpts Last

instance Monoid (HOpts Last) where
  mempty = HOpts mempty mempty mempty mempty mempty mempty mempty mempty

instance Semigroup (HOpts Last) where
  HOpts a b c d e f g h <> HOpts a' b' c' d' e' f' g' h'
     = HOpts (a <> a')
             (b <> b')
             (c <> c')
             (d <> d')
             (e <> e')
             (f <> f')
             (g <> g')
             (h <> h')

--seqPOptsM :: HOpts Last -> Maybe (HOpts Identity)
--seqPOptsM h = coerce (HOpts <$> oWidth h <*> oDebug h <*> oDisp h <*> oColor h)

-- | display format for the tree
data Disp = Ansi -- ^ draw normal tree
          | Unicode  -- ^ use unicode
          deriving (Show, Eq)

-- | default options
defOpts :: POpts
defOpts = HOpts
    { oWidth = 100
    , oDebug = DNormal
    , oDisp = Ansi
    , oColor = colorDef
    , oMsg = mempty
    , oRecursion = 100
    , oOther = otherDef
    , oNoColor = False
    }

-- | default title message color and boundaries between multipart refine messages
otherDef :: (Bool, SColor, SColor)
otherDef = coerce (True, Default, Default)

nocolor, colorDef :: (String, PColor)
nocolor = ("nocolor", PColor $ flip const)
colorDef = fromJust $ getLast $ oColor $ getOptT' @Color5

-- | how much detail to show in the expression tree
data Debug =
       DZero -- ^ one line summary used mainly for testing
     | DLite -- ^ one line summary with additional context from the head of the evaluation tree
     | DNormal  -- ^ outputs the evaluation tree but skips noisy subtrees
     | DVerbose -- ^ outputs the entire evaluation tree
     deriving (Ord, Show, Eq, Enum, Bounded)

-- | verbose debug flag
isVerbose :: POpts -> Bool
isVerbose = (DVerbose==) . oDebug

-- | color palettes
type Color1 = 'OColor "color1" 'Default 'Blue 'Default 'Red 'Black 'Cyan 'Black 'Yellow
type Color2 = 'OColor "color2" 'Default 'Magenta 'Default 'Red 'Black 'White 'Black 'Yellow
type Color3 = 'OColor "color3" 'Default 'Blue 'Red 'Default 'White 'Default 'Black 'Yellow
type Color4 = 'OColor "color4" 'Default 'Red 'Red 'Default 'Green 'Default 'Black 'Yellow
type Color5 = 'OColor "color5" 'Blue 'Default 'Red 'Default 'Cyan 'Default 'Yellow 'Default

type Other1 = 'OOther 'True 'Yellow 'Default
type Other2 = 'OOther 'True 'Default 'Default

-- | fix PresentT Bool to TrueT or FalseT
fixBoolT :: TT Bool -> TT Bool
fixBoolT t =
  case t ^? tBool . _PresentT of
    Nothing -> t
    Just b -> t & tBool .~ _boolT # b

show01 :: (Show a1, Show a2)
  => POpts
  -> String
  -> a1
  -> a2
  -> String
show01 opts msg0 ret = lit01 opts msg0 ret "" . show

show01' :: (Show a1, Show a2)
  => POpts
  -> String
  -> a1
  -> String
  -> a2
  -> String
show01' opts msg0 ret fmt = lit01 opts msg0 ret fmt . show

lit01 :: Show a1
  => POpts
  -> String
  -> a1
  -> String
  -> String
  -> String
lit01 opts msg0 ret fmt as
  | null fmt && null as = msg0
  | otherwise =
         msg0
      <> " "
      <> showL opts ret
      <> litVerbose opts (" | " ++ fmt) as

-- | more restrictive: only display data in verbose debug mode
litVerbose :: POpts
         -> String
         -> String
         -> String
litVerbose o = showLitImpl o DVerbose

showLitImpl :: POpts
            -> Debug
            -> String
            -> String
            -> String
showLitImpl o i s a =
  if oDebug o >= i || oDebug o == DLite then s <> litL o a
  else ""

showVerbose :: Show a
  => POpts
  -> String
  -> a
  -> String
showVerbose o = showAImpl o DVerbose

showAImpl :: Show a
  => POpts
  -> Debug
  -> String
  -> a
  -> String
showAImpl o i s a = showLitImpl o i s (show a)

showL :: Show a
  => POpts
  -> a
  -> String
showL o = litL o . show

litL :: POpts -> String -> String
litL = litL' . oWidth

litL' :: Int -> String -> String
litL' i s = take i s <> if length s > i then "..." else ""

litBL :: POpts -> BL8.ByteString -> String
litBL o s =
  let i = oWidth o
  in litL' i (BL8.unpack (BL8.take (fromIntegral i+1) s))

litBS :: POpts -> BS8.ByteString -> String
litBS o s =
  let i = oWidth o
  in litL' i (BS8.unpack (BS8.take (i+1) s))

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
  => POpts
  -> String
  -> String
  -> [Holder]
  -> Either (TT a) RH.Regex
compileRegex opts nm s hhs
  | null s = Left (mkNode opts (FailT "Regex cannot be empty") nm hhs)
  | otherwise =
      let rs = getROpts @rs
          mm = nm <> " " <> show rs
      in flip left (RH.compileM (TE.encodeUtf8 (T.pack s)) (snd rs))
            $ \e -> mkNode opts (FailT "Regex failed to compile") (mm <> ":" <> e) hhs

-- | extract the regex options from the type level list
class GetROpts (os :: [ROpt]) where
  getROpts :: ([String], [RL.PCREOption])
instance GetROpts '[] where
  getROpts = ([], [])
instance (Typeable r, GetROpt r, GetROpts rs) => GetROpts (r ': rs) where
  getROpts = ((showTK @r :) *** (getROpt @r :)) (getROpts @rs)

displayROpts :: [String] -> String
displayROpts xs = "[" <> (intercalate ", " (nub xs)) <> "]"

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

-- | simple regex string replacement options
data ReplaceFnSub = RPrepend | ROverWrite | RAppend deriving (Show,Eq)

-- | extract replacement options from typelevel
class GetReplaceFnSub (k :: ReplaceFnSub) where
  getReplaceFnSub :: ReplaceFnSub
instance GetReplaceFnSub 'RPrepend where getReplaceFnSub = RPrepend
instance GetReplaceFnSub 'ROverWrite where getReplaceFnSub = ROverWrite
instance GetReplaceFnSub 'RAppend where getReplaceFnSub = RAppend

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

-- | extract values from the trees or if there are errors return a tree with context
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

groupErrors :: [String] -> String
groupErrors =
     intercalate " | "
   . map (\xs@(x :| _) -> x <> (if length xs > 1 then "(" <> show (length xs) <> ")" else ""))
   . N.group

partitionTTExtended :: (w, TT a) -> Either ((w, TT x), String) (a, w, TT a)
partitionTTExtended (s, t) =
  case _tBool t of
    FailT e -> Left ((s, t & tBool .~ FailT e), e)
    PresentT a -> Right (a,s,t)
    TrueT -> Right (True,s,t)
    FalseT -> Right (False,s,t)

formatList :: forall x z . Show x
  => POpts
  -> [((Int, x), z)]
  -> String
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showAImpl opts DLite ", a=" a <> ")")

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

-- | 'FailT' prism
_FailT :: Prism' (BoolT a) String
_FailT = prism' FailT $ \case
                         FailT s -> Just s
                         _ -> Nothing

-- | 'PresentT' prism
_PresentT :: Prism' (BoolT a) a
_PresentT = prism' PresentT $ \case
                                PresentT a -> Just a
                                _ -> Nothing

-- | 'FalseT' prism
_FalseT :: Prism' (BoolT Bool) ()
_FalseT = prism' (const FalseT) $
            \case
               FalseT -> Just ()
               _ -> Nothing

-- | 'TrueT' prism
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
--
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

-- | helper method that fails with a msg when True
type family FailWhenT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailWhenT 'False _ = ()
  FailWhenT 'True e = GL.TypeError e

-- | helper method that fails with msg when False
type family FailUnlessT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailUnlessT 'True _ = ()
  FailUnlessT 'False e = GL.TypeError e

-- | typelevel boolean And
type family AndT (b :: Bool) (b1 :: Bool) :: Bool where
  AndT 'False _ = 'False
  AndT 'True b1 = b1

-- | typelevel boolean Or
type family OrT (b :: Bool) (b1 :: Bool) :: Bool where
  OrT 'True _ = 'True
  OrT 'False b1 = b1

-- | typelevel boolean Not
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

-- | display constructor name for 'These'
showThese :: These a b -> String
showThese = \case
  This {} -> "This"
  That {} -> "That"
  These {} -> "These"

-- | get 'These' from typelevel
class GetThese th where
  getThese :: (String, These w v -> Bool)
instance GetThese ('This x) where
  getThese = ("This", isThis)
instance GetThese ('That y) where
  getThese = ("That", isThat)
instance GetThese ('These x y) where
  getThese = ("These", isThese)

-- | get ordering from the typelevel
class GetOrdering (cmp :: Ordering) where
  getOrdering :: Ordering
instance GetOrdering 'LT where
  getOrdering = LT
instance GetOrdering 'EQ where
  getOrdering = EQ
instance GetOrdering 'GT where
  getOrdering = GT

-- | get 'Bool' from the typelevel
class GetBool (a :: Bool) where
  getBool :: Bool
instance GetBool 'True where
  getBool = True
instance GetBool 'False where
  getBool = False

-- | get 'Color' from the typelevel
class GetColor (a :: Color) where
  getColor :: Color
instance GetColor 'Black where
  getColor = Black
instance GetColor 'Red where
  getColor = Red
instance GetColor 'Green where
  getColor = Green
instance GetColor 'Yellow where
  getColor = Yellow
instance GetColor 'Blue where
  getColor = Blue
instance GetColor 'Magenta where
  getColor = Magenta
instance GetColor 'Cyan where
  getColor = Cyan
instance GetColor 'White where
  getColor = White
instance GetColor 'Default where
  getColor = Default

-- | all the ways to compare two values
data OrderingP = CGt | CGe | CEq | CLe | CLt | CNe deriving (Show, Eq, Enum, Bounded)

-- | extract 'OrderingP' from the typelevel
class GetOrd (k :: OrderingP) where
  getOrd :: Ord a => (String, a -> a -> Bool)

instance GetOrd 'CGt where getOrd = (">", (>))
instance GetOrd 'CGe where getOrd = (">=",(>=))
instance GetOrd 'CEq where getOrd = ("==",(==))
instance GetOrd 'CLe where getOrd = ("<=",(<=))
instance GetOrd 'CLt where getOrd = ("<", (<))
instance GetOrd 'CNe where getOrd = ("/=",(/=))

-- | pretty print a tree
toNodeString :: POpts
             -> PE
             -> String
toNodeString opts bpe =
  if hasNoTree opts
  then errorInProgram $ "shouldnt be calling this if we are dropping details: toNodeString " <> show (oDebug opts) <> " " <> show bpe
  else colorBoolP opts (_pBool bpe) <> " " <> _pString bpe

hasNoTree :: POpts -> Bool
hasNoTree opts =
  case oDebug opts of
    DZero -> True
    DLite -> True
    DNormal -> False
    DVerbose -> False

nullSpace :: String -> String
nullSpace s | null s = ""
            | otherwise = " " <> s

-- | render the 'BoolP' value with colors
colorBoolP ::
     POpts
  -> BoolP
  -> String
colorBoolP o =
  \case
    b@(FailP e) -> "[" <> colorMe o b "Error" <> nullSpace e <> "]"
    b@PresentP -> colorMe o b "P"
    b@TrueP -> colorMe o b "True"
    b@FalseP -> colorMe o b "False"

-- | render the 'BoolT' value with colors
colorBoolT :: Show a
    => POpts
    -> BoolT a
    -> String
colorBoolT o r =
  let f = colorMe o (r ^. boolT2P)
  in case r of
      FailT e -> f "Error" <> " " <> e
      TrueT -> f "True"
      FalseT -> f "False"
      PresentT x -> f "Present" <> " " <> show x

colorBoolT' :: Show a
   => POpts
   -> BoolT a
   -> String
colorBoolT' o r =
  let f = colorMe o (r ^. boolT2P)
  in case r of
      FailT e -> f "FailT" <> " " <> e
      TrueT -> f "TrueT"
      FalseT -> f "FalseT"
      PresentT x -> f "PresentT" <> " " <> show x

-- | colors the result of the predicate based on the current color palette
colorMe ::
     POpts
  -> BoolP
  -> String
  -> String
colorMe o b s =
  let (_, PColor f) = if oNoColor o then nocolor else oColor o
  in f b s

fixLite :: forall a . Show a
   => POpts
   -> a
   -> Tree PE
   -> String
fixLite opts a t
  | hasNoTree opts = fixPresentP opts (t ^. root . pBool) a <> "\n"
  | otherwise = prtTreePure opts t

fixPresentP :: Show a
  => POpts
  -> BoolP
  -> a
  -> String
fixPresentP opts bp a =
  case bp of
    PresentP -> colorMe opts PresentP "Present" <> " " <> show a
    _ -> colorBoolP opts bp

prtTreePure ::
     POpts
  -> Tree PE
  -> String
prtTreePure opts t
  | hasNoTree opts = colorBoolP opts (t ^. root . pBool)
  | otherwise = showImpl opts $ fmap (toNodeString opts) t

topMessage :: TT a -> String
topMessage pp =
  let s = pp ^. tString
  in if null s then "" else "(" <> s <> ")"

showImpl :: POpts
         -> Tree String
         -> String
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

-- | show the type as a string
showT :: forall (t :: Type) . Typeable t => String
showT = show (typeRep (Proxy @t))

-- | show the kind as a string
showTK :: forall r . Typeable r => String
showTK = show (typeRep (Proxy @r))

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
instance InductListC 1 a where
  type InductListP 1 a = (a,())
  inductListC [a] = (a,())
  inductListC _ = errorInProgram "inductListC: expected 1 value"
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

-- composite types are used instead of type synonyms as showT (typeRep) unrolls the definition
-- eg sqlhandler.encode/decode and parsejson* etc
-- | Display options
data OptT =
    OWidth !Nat           -- ^ set display width
  | OMsg !Symbol          -- ^ set text to add context to a failure message for refined types
  | ORecursion !Nat       -- ^ set recursion limit eg for regex
  | OOther                -- ^ set effects for messages
     !Bool    -- ^ set underline
     !Color   -- ^ set foreground color
     !Color   -- ^ set background color
  | OEmpty                -- ^ mempty
  | !OptT :# !OptT        -- ^ mappend
  | OColor    -- ^ set color palette
     !Symbol  -- ^ name of color palette
     !Color   -- ^ Fail foreground color
     !Color   -- ^ Fail background color
     !Color   -- ^ False foreground color
     !Color   -- ^ False background color
     !Color   -- ^ True foreground color
     !Color   -- ^ True background color
     !Color   -- ^ Present foreground color
     !Color   -- ^ Present background color
  | OColorOn  -- ^ turn on colors
  | OColorOff -- ^ turn off colors
  | OAnsi                 -- ^ ansi display
  | OUnicode              -- ^ unicode display
  | OZero                 -- ^ debug mode return nothing
  | OLite                 -- ^ debug mode return one line
  | ONormal               -- ^ debug mode normal
  | OVerbose              -- ^ debug mode verbose
  | OZ                    -- ^ composite: no messages
  | OL                    -- ^ composite: lite version
  | OAN                   -- ^ composite: ansi + no colors
  | OANV                  -- ^ composite: ansi + no colors + verbose
  | OA                    -- ^ composite: ansi + colors
  | OAV                   -- ^ composite: ansi + colors + verbose
  | OAB                   -- ^ composite: ansi + colors + background
  | OU                    -- ^ composite: unicode + colors
  | OUB                   -- ^ composite: unicode + colors + background
  | OUV                   -- ^ composite: unicode + colors + verbose

instance Show OptT where
  show = \case
            OWidth _n -> "OWidth"
            OMsg _s -> "OMsg"
            ORecursion _n -> "ORecursion"
            OOther _b _c1 _c2 -> "OOther"
            OEmpty -> "OEmpty"
            a :# b -> show a ++ " ':# " ++ show b
            OColor _s _c1 _c2 _c3 _c4 _c5 _c6 _c7 _c8 -> "OColor"
            OColorOn -> "OColorOn"
            OColorOff -> "OColorOff"
            OAnsi -> "OAnsi"
            OUnicode -> "OUnicode"
            OZero -> "OZero"
            OLite -> "OLite"
            ONormal -> "ONormal"
            OVerbose -> "OVerbose"
            OZ -> "OZ"
            OL -> "OL"
            OAN -> "OAN"
            OANV -> "OANV"
            OA -> "OA"
            OAB -> "OAB"
            OAV -> "OAV"
            OU -> "OU"
            OUB -> "OUB"
            OUV -> "OUV"

infixr 6 :#

class OptTC (k :: OptT) where
   getOptT' :: POptsL
instance KnownNat n => OptTC ('OWidth n) where
   getOptT' = setWidth (nat @n)
instance KnownSymbol s => OptTC ('OMsg s) where
   getOptT' = setMessage (symb @s)
instance KnownNat n => OptTC ('ORecursion n) where
   getOptT' = setRecursion (nat @n)
instance ( GetBool b
         , GetColor c1
         , GetColor c2
         ) => OptTC ('OOther b c1 c2) where
   getOptT' = setOther (getBool @b) (getColor @c1) (getColor @c2)
instance OptTC 'OEmpty where
   getOptT' = mempty
instance ( OptTC a
         , OptTC b
         ) => OptTC (a ':# b) where
   getOptT' = getOptT' @a <> getOptT' @b
instance ( KnownSymbol s
         , GetColor c1
         , GetColor c2
         , GetColor c3
         , GetColor c4
         , GetColor c5
         , GetColor c6
         , GetColor c7
         , GetColor c8)
  => OptTC ('OColor s c1 c2 c3 c4 c5 c6 c7 c8) where
     getOptT' = setCreateColor
        (symb @s)
        (getColor @c1)
        (getColor @c2)
        (getColor @c3)
        (getColor @c4)
        (getColor @c5)
        (getColor @c6)
        (getColor @c7)
        (getColor @c8)
instance OptTC 'OColorOn where
   getOptT' = setNoColor False
instance OptTC 'OColorOff where
   getOptT' = setNoColor True
instance OptTC 'OAnsi where
   getOptT' = setDisp Ansi
instance OptTC 'OUnicode where
   getOptT' = setDisp Unicode
instance OptTC 'OZero where
   getOptT' = setDebug DZero
instance OptTC 'OLite where
   getOptT' = setDebug DLite
instance OptTC 'ONormal where
   getOptT' = setDebug DNormal
instance OptTC 'OVerbose where
   getOptT' = setDebug DVerbose
instance OptTC 'OZ where
   getOptT' = setDisp Ansi <> setNoColor True <> setDebug DZero
instance OptTC 'OL where
   getOptT' = setDisp Ansi <> setNoColor True <> setDebug DLite <> setWidth 200
instance OptTC 'OAN where
   getOptT' = setDisp Ansi <> setNoColor True <> setDebug DNormal <> setWidth 100
instance OptTC 'OANV where
   getOptT' = setDisp Ansi <> setNoColor True <> setDebug DVerbose <> setWidth 200
instance OptTC 'OA where
   getOptT' = setDisp Ansi <> getOptT' @Color5 <> setDebug DNormal <> getOptT' @Other2 <> setWidth 100
instance OptTC 'OAB where
   getOptT' = setDisp Ansi <> getOptT' @Color1 <> setDebug DNormal <> getOptT' @Other1 <> setWidth 100
instance OptTC 'OAV where
   getOptT' = getOptT' @('OA ':# 'OVerbose ':# 'OWidth 200)
instance OptTC 'OU where
   getOptT' = getOptT' @('OA ':# 'OUnicode)
instance OptTC 'OUB where
   getOptT' = getOptT' @('OAB ':# 'OUnicode)
instance OptTC 'OUV where
   getOptT' = getOptT' @('OAV ':# 'OUnicode)

type OZ = 'OAnsi ':# 'OColorOff ':# 'OZero
type OL = 'OAnsi ':# 'OColorOff ':# 'OLite ':# 'OWidth 200
type OAN = 'OAnsi ':# 'OColorOff ':# 'ONormal ':# 'OWidth 100
type OANV = 'OAnsi ':# 'OColorOff ':# 'OVerbose ':# 'OWidth 200
type OA = 'OAnsi ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
type OAB = 'OAnsi ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
type OAV = 'OAnsi ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200
type OU = 'OUnicode ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
type OUB = 'OUnicode ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
type OUV = 'OUnicode ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200

-- | convert typelevel options to 'POpts'
--
-- >>> (oDisp &&& fst . oColor &&& oWidth) (getOptT @(OA ':# OU ':# OA ':# 'OWidth 321 ':# Color4 ':# 'OMsg "test message"))
-- (Ansi,("color4",321))
--
-- >>> oMsg (getOptT @('OMsg "abc" ':# 'OMsg "def"))
-- ["abc","def"]
--
-- >>> oOther (getOptT @('OOther 'False 'Red 'White ':# 'OOther 'True 'Red 'Black))
-- (True,Red,Black)
--
-- >>> a = show (getOptT @('OEmpty ':# OU))
-- >>> b = show (getOptT @(OU ':# 'OEmpty));
-- >>> c = show (getOptT @OU)
-- >>> a==b && b==c
-- True
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

chkSize :: Foldable t
   => POpts
   -> String
   -> t a
   -> [Holder]
   -> Either (TT x) ()
chkSize opts msg0 xs hhs =
  let mx = oRecursion opts
  in case splitAt mx (toList xs) of
    (_,[]) -> Right ()
    (_,_:_) -> Left $ mkNode opts (FailT (msg0 <> " list size exceeded")) ("max is " ++ show mx) hhs

formatOMsg :: POpts -> String -> String
formatOMsg o suffix =
  case oMsg o of
    [] -> mempty
    s@(_:_) -> intercalate " | " (map (setOtherEffects o) s) <> suffix

subopts :: POpts -> POpts
subopts opts =
  case oDebug opts of
    DZero -> opts { oDebug = DLite }
    _ -> opts

setOtherEffects :: POpts -> String -> String
setOtherEffects o =
  if oNoColor o then id
  else case coerce (oOther o) of
         (False, Default, Default) -> id
         (b, c1, c2) -> (if b then style Underline else id) . color c1 . bgColor c2

pureTryTest :: a -> IO (Either () a)
pureTryTest = fmap (left (const ())) . E.try @E.SomeException . E.evaluate

pureTryTestPred :: (String -> Bool)
                -> a
                -> IO (Either String (Either () a))
pureTryTestPred p a = do
  lr <- left E.displayException <$> E.try @E.SomeException (E.evaluate a)
  return $ case lr of
    Left e | p e -> Right (Left ())
           | otherwise -> Left ("no match found: e=" ++ e)
    Right r -> Right (Right r)

-- | prime predicate
--
-- >>> isPrime 7
-- True
--
-- >>> isPrime 6
-- False
--
isPrime :: Int -> Bool
isPrime n = n==2 || n>2 && all ((> 0).rem n) (2:[3,5 .. floor . sqrt @Double . fromIntegral $ n+1])

type family AnyT :: k where {}

-- | mconcat  options at the type level
--
-- >>> x = getOptT @(OptTT '[ 'OMsg "test", 'ORecursion 123, OU, OL, 'OMsg "field2"])
-- >>> oMsg x
-- ["test","field2"]
-- >>> oRecursion x
-- 123
--
type family OptTT (xs :: [OptT]) where
  OptTT '[] = 'OEmpty
  OptTT (x ': xs) = x ':# OptTT xs