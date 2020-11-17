{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | utility methods for Predicate / methods for displaying the evaluation tree
module Predicate.Util (
 -- ** Val
    Val(..)
  , _Fail
  , _Val
  , _ValEither

  -- ** TT typed tree
  , TT
  , ttVal
  , ttValBool
  , ttString
  , ttForest

 -- ** PE untyped tree
  , PE(..)
  , peValP
  , peString

 -- ** ValP
  , ValP(..)
  , _FailP
  , _TrueP
  , _FalseP
  , _ValP

 -- ** create tree
  , mkNode
  , mkNodeB
  , mkNodeCopy

 -- ** tree manipulation
  , getValAndPE
  , getValLRFromTT
  , getValueLR
  , Inline (..)
  , prefixNumberToTT
  , prefixMsg
  , splitAndAlign
  , verboseList
  , fixTTBool
  , topMessage
  , hasNoTree

 -- ** options
  , POpts
  , Debug(..)
  , Disp(..)
  , Color(..)
  , isVerbose
  , colorValBool
  , colorValP
  , Long(..)
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
  , type OA
  , type OAB
  , type OAN
  , type OAV
  , type OANV
  , type OU
  , type OUB
  , type OUN
  , type OUV
  , type OUNV

  , HOpts(..)
  , Opt(..)
  , OptC
  , type OptT
  , getOpt
  , zeroToLite
  , defOpts

-- ** formatting functions
  , show3
  , show3'
  , lit3
  , litVerbose
  , showVerbose
  , showL
  , litL
  , litBL
  , litBS
  , joinStrings

 -- ** printing methods
  , prtTreePure
  , formatOMsg
  , prtTree

 -- ** MonadEval
  , MonadEval(..)

 -- ** miscellaneous
  , hh
  , chkSize
  , chkSize2
  , badLength
  , getMaxRecursionValue
  , lengthGreaterThanOne
  ) where
import Predicate.Misc
import GHC.TypeLits (Symbol, Nat, KnownSymbol, KnownNat)
import Control.Lens
import Control.Arrow (Arrow((&&&)), ArrowChoice(left))
import Data.List (intercalate, isInfixOf)
import Data.Tree (drawTree, Forest, Tree(Node))
import Data.Tree.Lens (root)
import System.Console.Pretty (Color(..))
import qualified System.Console.Pretty as C
import qualified Control.Exception as E
import Control.DeepSeq (NFData, ($!!))
import System.IO.Unsafe (unsafePerformIO)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N
import Data.Either (partitionEithers)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid (Last(Last))
import Data.Maybe (fromMaybe)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import qualified Safe (initSafe, fromJustNote)
import Control.Monad (ap)
import Data.Bool (bool)
import GHC.Generics (Generic, Generic1)
import qualified Language.Haskell.TH.Lift as TH
import Instances.TH.Lift ()
import Data.Kind (Type)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :m + Control.Arrow

-- | contains the untyped result from evaluating an expression
data ValP =
    FailP !String -- ^ evaluation failed
  | FalseP       -- ^ False predicate
  | TrueP        -- ^ True predicate
  | ValP     -- ^ Any value
  deriving stock (Show, Ord, Eq, Read, Generic)
  deriving TH.Lift

makePrisms ''ValP

-- | untyped child node for 'TT'
data PE = PE { _peValP :: !ValP -- ^ holds the result of running the predicate
             , _peString :: !String -- ^ optional strings to include in the results
             } deriving stock (Show, Read, Eq, Generic)
               deriving TH.Lift

makeLenses ''PE

instance Monoid PE where
  mempty = PE mempty mempty

-- | concatenate two strings with delimiter
--
-- >>> joinStrings "xyz" "abc"
-- "xyz | abc"
--
-- >>> joinStrings "" "abc"
-- "abc"
--
-- >>> joinStrings "xyz" ""
-- "xyz"
--
-- >>> joinStrings "" ""
-- ""
--
joinStrings :: String -> String -> String
joinStrings s s1 = s <> (if null s || null s1 then "" else " | ") <> s1

instance Semigroup PE where
  PE b s <> PE b1 s1 = PE (b <> b1) (joinStrings s s1)

-- | semigroup for ValP
--
-- >>> TrueP <> FalseP <> ValP
-- FalseP
--
-- >>> ValP <> TrueP <> FalseP
-- FalseP
--
-- >>> ValP <> TrueP <> ValP
-- TrueP
--
-- >>> FailP "abc" <> (TrueP <> FalseP) <> FailP "def"
-- FailP "abc | def"
--
-- >>> (FailP "abc" <> TrueP) <> (FalseP <> FailP "def")
-- FailP "abc | def"
--
-- >>> FailP "" <> (TrueP <> FalseP) <> FailP "def"
-- FailP "def"
--
-- >>> FailP "abc" <> FailP "" <> FailP "def"
-- FailP "abc | def"
--
-- >>> FailP "abc" <> FailP "xyz" <> FailP "def"
-- FailP "abc | xyz | def"
--
instance Semigroup ValP where
   FailP s <> FailP s1 = FailP (joinStrings s s1)
   FailP s <> _ = FailP s
   _ <> FailP s = FailP s
   FalseP <> _ = FalseP
   _ <> FalseP = FalseP
   TrueP <> _ = TrueP
   _ <> TrueP = TrueP
   ValP <> ValP = ValP

instance Monoid ValP where
  mempty = ValP

-- | contains the typed result from evaluating an expression
data Val a = Fail !String | Val !a
  deriving stock (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Generic, Generic1)
  deriving TH.Lift

makePrisms ''Val

instance Applicative Val where
  pure = Val
  (<*>) = ap

instance Monad Val where
  return = pure
  Val a >>= amb = amb a
  Fail s >>= _ = Fail s

-- | semigroup instance for 'Val'
--
-- >>> Val 123 <> (Val 456 <> Val 789) == (Val 123 <> Val 456) <> Val 789
-- True
--
-- >>> Val True <> Val False
-- Val False
--
-- >>> Val True <> Val True
-- Val True
--
-- >>> Fail "abc" <> (Val True <> Val False) <> Fail "def"
-- Fail "abc | def"
--
-- >>> (Fail "abc" <> Val True) <> (Val False <> Fail "def")
-- Fail "abc | def"
--
-- >>> Fail "" <> (Val True <> Val False) <> Fail "def"
-- Fail "def"
--
-- >>> Fail "abc" <> Fail "" <> Fail "def"
-- Fail "abc | def"
--
-- >>> Val False <> (Val True <> Val False) == (Val False <> Val True) <> Val False
-- True
--
instance Semigroup (Val a) where
   Fail s <> Fail s1 = Fail (joinStrings s s1)
   Fail s <> _ = Fail s
   _ <> Fail s = Fail s
   Val _ <> Val b = Val b

-- | monoid instance for 'Val'
--
-- >>> mempty :: Val (Maybe [Int])
-- Val Nothing
--
-- >>> import qualified Data.Semigroup as SG
-- >>> mempty :: SG.Sum Int
-- Sum {getSum = 0}
--
instance Monoid a => Monoid (Val a) where
   mempty = Val mempty

-- | 'Read' instance for Val
--
-- >>> reads @(Val Int) "Val 123"
-- [(Val 123,"")]
--
-- >>> reads @(Val Bool) "Val False abc"
-- [(Val False," abc")]
--
-- >>> reads @(Val Bool) "Fail \"some error message\""
-- [(Fail "some error message","")]
--
-- >>> reads @(Val Double) "Fail \"some error message\""
-- [(Fail "some error message","")]
--

-- | typed tree holding the results of evaluating a type level expression
data TT a = TT { _ttValP :: !ValP -- ^ display value
               , _ttVal :: !(Val a)  -- ^ the value at this root node
               , _ttString :: !String  -- ^ detailed information eg input and output and text
               , _ttForest :: !(Forest PE) -- ^ the child nodes
               } deriving stock (Functor, Read, Show, Eq, Foldable, Traversable, Generic, Generic1)
                 deriving stock TH.Lift

-- dont expose lenses for _ttValP and _ttVal as they must be kept in sync: see ttVal
makeLensesFor [("_ttString","ttString"),("_ttForest","ttForest")] ''TT

instance Semigroup (TT a) where
   TT bp bt ss ts <> TT bp1 bt1 ss1 ts1 =
     TT (bp <> bp1) (bt <> bt1) (joinStrings ss ss1) (ts <> ts1)

instance Monoid a => Monoid (TT a) where
   mempty = TT mempty mempty mempty mempty

instance Applicative TT where
  pure a = TT ValP (Val a) "" []
  (<*>) = ap

fixTTValP :: ValP -> TT a -> TT a
fixTTValP bp tt = tt { _ttValP = bp <> _ttValP tt }

instance Monad TT where
  return = pure
  z@(TT bp bt ss ts) >>= amb =
     case bt of
       Val a -> fixTTValP bp $ amb a
                  & ttString %~ joinStrings ss
                  & ttForest %~ (ts <>)
       Fail e -> z { _ttVal = Fail e }

-- | creates a Node for the evaluation tree
mkNodeCopy :: POpts
       -> TT a
       -> String
       -> [Tree PE]
       -> TT a
mkNodeCopy opts tt msg pes =
  mkNodeImpl opts (_ttValP tt, _ttVal tt) msg (pes <> [hh tt])

-- | creates a Node for the evaluation tree
mkNode :: POpts
       -> Val a
       -> String
       -> [Tree PE]
       -> TT a
mkNode opts = mkNodeImpl opts . (val2P &&& id)

-- | creates a Node for the evaluation tree
mkNodeImpl :: POpts
           -> (ValP, Val a)
           -> String
           -> [Tree PE]
           -> TT a
mkNodeImpl opts (bp',bt) ss hs =
  let bp = validateValP bp' bt
  in case oDebug opts of
      DZero -> TT bp bt "" []
      DLite ->
      -- keeps the last string so we can use the root to give more details on failure (especially for Refined* types)
      -- also holds onto any failures
          let zs = filter (has (root . peValP . _FailP)) hs
          in TT bp bt ss zs
      _ -> TT bp bt ss hs

-- | check that the 'ValP' value is consistent with 'Val'
validateValP :: ValP -> Val a -> ValP
validateValP bp bt =
  case bt of
    Val _a -> case bp of
                     FailP e -> errorInProgram $ "validateValP: found Val and FailP e=" ++ e
                     _ -> bp
    Fail e -> case bp of
                FailP e1 | e==e1 -> bp
                         | otherwise -> errorInProgram $ "validateValP: found Fail and FailP but message mismatch in FailP " ++ show (e,e1)
                _ -> errorInProgram $ "validateValP: found " ++ show bp ++ " expected FailP e=" ++ e

-- | fix the 'ValP' value for the Bool case: ie use 'TrueP' and 'FalseP'
--
-- >>> fixTTBool (TT ValP (Val True) "x" []) == TT TrueP (Val True) "x" []
-- True
--
-- >>> fixTTBool (TT FalseP (Fail "abc") "x" []) == TT (FailP "abc") (Fail "abc") "x" []
-- True
--
fixTTBool :: TT Bool -> TT Bool
fixTTBool = over ttValBool id

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts
        -> Bool
        -> String
        -> [Tree PE]
        -> TT Bool
mkNodeB opts = mkNodeImpl opts . (bool FalseP TrueP &&& Val)

-- | convenience method to pull parts out of 'TT'
getValAndPE :: TT a -> (Either String a, Tree PE)
getValAndPE = getValLRFromTT &&& hh

-- | convenience method to pull out the return value from 'TT'
getValLRFromTT :: TT a -> Either String a
getValLRFromTT = view (ttVal . _ValEither)

-- | converts a typed tree to an untyped tree for display
hh :: TT a -> Tree PE
hh (TT bp bt ss tt) = Node (PE (validateValP bp bt) ss) tt

-- | flag for deciding whether to inline a tree or extend it
data Inline = Inline | NoInline deriving (Show, Eq)

-- | decorate the tree with more detail when there are errors but inline the error node
getValueLR :: Inline
           -> POpts
           -> String
           -> TT a
           -> [Tree PE]
           -> Either (TT x) a
getValueLR inline opts msg0 tt hs =
-- hack: if infix ...
  let ts = if _ttString tt `isInfixOf` msg0 then "" else _ttString tt
      xs = joinStrings ts msg0
      tts = case inline of
              Inline -> hs <> _ttForest tt
              NoInline -> hs <> [hh tt]
  in left (\e -> mkNode opts (Fail e) xs tts) (getValLRFromTT tt)


-- | elide the 'Identity' wrapper so it acts like a normal ADT
type family HKD (f :: Type -> Type) (a :: Type) where
  HKD Identity a = a
  HKD f a = f a

-- | final set of options using Identity
type POpts = HOpts Identity

-- | customizable options for running a typelevel expression
data HOpts f =
  HOpts { oWidth :: !(HKD f Int) -- ^ length of data to display for 'showLitImpl'
        , oDebug :: !(HKD f Debug) -- ^ debug level
        , oDisp :: !(HKD f Disp) -- ^ display the tree using the normal tree or unicode
        , oColor :: !(HKD f (String, PColor)) -- ^ color palette used
        , oMsg :: ![String] -- ^ messages associated with type
        , oRecursion :: !(HKD f Int) -- ^ max recursion for small values
        , oRecursionLarge :: !(HKD f Int) -- ^ max recursion for large values eg for Text
        , oLarge :: !(HKD f Bool) -- ^ use large value recursion
        , oOther :: !(HKD f (Bool, SColor, SColor)) -- ^ other message effects
        , oNoColor :: !(HKD f Bool) -- ^ no colors
        }

-- | the color palette for displaying the expression tree
newtype PColor = PColor (ValP -> String -> String)
instance Show PColor where
  show PColor {} = "PColor <fn>"

deriving stock instance
  ( Show (HKD f Int)
  , Show (HKD f Debug)
  , Show (HKD f Disp)
  , Show (HKD f (String, PColor))
  , Show (HKD f Bool)
  , Show (HKD f (Bool, SColor, SColor))
  ) => Show (HOpts f)

-- | convert to a usable option using defaults to fill in any gaps
reifyOpts :: HOpts Last -> HOpts Identity
reifyOpts h =
  HOpts (fromMaybe (oWidth defOpts) (coerce (oWidth h)))
        (fromMaybe (oDebug defOpts) (coerce (oDebug h)))
        (fromMaybe (oDisp defOpts) (coerce (oDisp h)))
        (if fromMaybe (oNoColor defOpts) (coerce (oNoColor h))
           then nocolor
           else fromMaybe (oColor defOpts) (coerce (oColor h))
        )
        (oMsg defOpts <> oMsg h)
        (fromMaybe (oRecursion defOpts) (coerce (oRecursion h)))
        (fromMaybe (oRecursionLarge defOpts) (coerce (oRecursionLarge h)))
        (fromMaybe (oLarge defOpts) (coerce (oLarge h)))
        (if fromMaybe (oNoColor defOpts) (coerce (oNoColor h))
           then otherDef
           else fromMaybe (oOther defOpts) (coerce (oOther h))
        )
        (fromMaybe (oNoColor defOpts) (coerce (oNoColor h)))

-- | set maximum display width of expressions
setWidth :: Int -> HOpts Last
setWidth i = mempty { oWidth = pure i }

-- | set title message for the display tree
setMessage :: String -> HOpts Last
setMessage s = mempty { oMsg = pure s }

-- | set maximum recursion for small values eg when running regex
setRecursion :: Int -> HOpts Last
setRecursion i = mempty { oRecursion = pure i }

-- | set maximum recursion for large values
setRecursionLarge :: Int -> HOpts Last
setRecursionLarge i = mempty { oRecursionLarge = pure i }

-- | set maximum recursion eg when running regex
setLarge :: Bool -> HOpts Last
setLarge b = mempty { oLarge = pure b }

-- | set color of title message
setOther :: Bool
         -> Color
         -> Color
         -> HOpts Last
setOther b c1 c2 = mempty { oOther = pure $ coerce (b, c1, c2) }

-- | turn on/off colors
setNoColor :: Bool -> HOpts Last
setNoColor b = mempty { oNoColor = pure b }

-- | display type eg 'Unicode' or 'Ansi'
setDisp :: Disp -> HOpts Last
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
   -> HOpts Last
setCreateColor s c1 c2 c3 c4 c5 c6 c7 c8 =
  let pc = \case
       FailP {} -> C.color c1 . C.bgColor c2
       FalseP -> C.color c3 . C.bgColor c4
       TrueP -> C.color c5 . C.bgColor c6
       ValP -> C.color c7 . C.bgColor c8
  in mempty { oColor = pure (s,PColor pc) }

-- | set debug mode
setDebug :: Debug -> HOpts Last
setDebug d =
  mempty { oDebug = pure d }

instance Monoid (HOpts Last) where
  mempty = HOpts mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Semigroup (HOpts Last) where
  HOpts a b c d e f g h i j <> HOpts a' b' c' d' e' f' g' h' i' j'
     = HOpts (a <> a')
             (b <> b')
             (c <> c')
             (d <> d')
             (e <> e')
             (f <> f')
             (g <> g')
             (h <> h')
             (i <> i')
             (j <> j')

--seqPOptsM :: HOpts Last -> Maybe (HOpts Identity)
--seqPOptsM h = coerce (HOpts <$> oWidth h <*> oDebug h <*> oDisp h <*> oColor h)

-- | display format for the tree
data Disp = Ansi -- ^ draw normal tree
          | Unicode  -- ^ use unicode
          deriving stock (Show, Eq, Read, Bounded, Enum)

-- | default options
defOpts :: POpts
defOpts = HOpts
    { oWidth = 100
    , oDebug = DNormal
    , oDisp = Ansi
    , oColor = colorDef
    , oMsg = mempty
    , oRecursion = 100
    , oRecursionLarge = 10_000
    , oLarge = False
    , oOther = otherDef
    , oNoColor = False
    }

-- | default title message color and boundaries between multipart refine messages
otherDef :: (Bool, SColor, SColor)
otherDef = coerce (True, Default, Default)

nocolor, colorDef :: (String, PColor)
-- | skip colors
nocolor = ("nocolor", PColor $ const id)
-- | use default coloring from 'Color5'
colorDef = Safe.fromJustNote "colorDef" $ coerce $ oColor $ getOptC @Color5

-- | how much detail to show in the expression tree
data Debug =
       DZero -- ^ one line summary used mainly for testing
     | DLite -- ^ one line summary with additional context from the top of the evaluation tree
     | DNormal  -- ^ outputs the evaluation tree but skips noisy subtrees
     | DVerbose -- ^ outputs the entire evaluation tree
     deriving stock (Read, Ord, Show, Eq, Enum, Bounded)

-- | verbose debug flag
isVerbose :: POpts -> Bool
isVerbose = (DVerbose==) . oDebug

-- | color palette for general display
type Color1 = 'OColor "color1" 'Default 'Blue 'Default 'Red 'Black 'Cyan 'Black 'Yellow
-- | color palette for general display
type Color2 = 'OColor "color2" 'Default 'Magenta 'Default 'Red 'Black 'White 'Black 'Yellow
-- | color palette for general display
type Color3 = 'OColor "color3" 'Default 'Blue 'Red 'Default 'White 'Default 'Black 'Yellow
-- | color palette for general display
type Color4 = 'OColor "color4" 'Default 'Red 'Red 'Default 'Green 'Default 'Black 'Yellow
-- | color palette for general display
type Color5 = 'OColor "color5" 'Blue 'Default 'Red 'Default 'Cyan 'Default 'Yellow 'Default

-- | color palette for effects used in 'Predicate.Core.Msg' and the refined modules
type Other1 = 'OOther 'True 'Yellow 'Default
-- | color palette for effects used in 'Predicate.Core.Msg' and the refined modules
type Other2 = 'OOther 'True 'Default 'Default

-- | display a message and three values where the last one is displayed in verbose mode
show3 :: (Show a1, Show a2)
  => POpts
  -> String
  -> a1
  -> a2
  -> String
show3 opts msg0 ret = lit3 opts msg0 ret "" . show

-- | display a message and four values where the last two are displayed in verbose mode
show3' :: (Show a1, Show a2)
  => POpts
  -> String
  -> a1
  -> String
  -> a2
  -> String
show3' opts msg0 ret fmt = lit3 opts msg0 ret fmt . show

-- | display a message and four values where the last two are displayed in verbose mode
lit3 :: Show a1
  => POpts
  -> String
  -> a1
  -> String
  -> String
  -> String
lit3 opts msg0 ret fmt as
  | null fmt && null as = msg0
  | otherwise =
         msg0
      <> (if null msg0 then "" else " ")
      <> showL opts ret
      <> litVerbose opts (" | " <> litL opts fmt) as

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
  if oDebug o >= i || oDebug o == DLite then litL o s <> litL o a
  else ""

-- | show the value in verbose mode
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

-- | display a showable value limited to the width found in the options
showL :: Show a
  => POpts
  -> a
  -> String
showL o = litL o . show

-- | display a value limited to the width found in the options
litL :: POpts -> String -> String
litL = litL' . oWidth

litL' :: Int -> String -> String
litL' i s =
  let (z,e) = splitAt i s
  in z ++ if null e then "" else "..."

-- | display lazy bytestring output up to the maximum width in the options
litBL :: POpts -> BL8.ByteString -> String
litBL o s =
  let i = oWidth o
  in litL' i (BL8.unpack (BL8.take (1+fromIntegral i) s))

-- | display bytestring output up to the maximum width in the options
litBS :: POpts -> BS8.ByteString -> String
litBS o s =
  let i = oWidth o
  in litL' i (BS8.unpack (BS8.take (i+1) s))

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
                       (Fail (groupErrors (map snd excs)))
                       (msgs <> (formatList opts [fst e] <> " excnt=" <> show (length excs)))
                       (map (hh . snd) ts)
     ([], tfs) -> Right tfs

groupErrors :: [String] -> String
groupErrors =
     intercalate " | "
   . map (\xs@(x :| _) -> x <> let ll = length xs in (if ll > 1 then "(" <> show ll <> ")" else ""))
   . N.group

-- | returns True if the list has more than one element
lengthGreaterThanOne :: [a] -> Bool
lengthGreaterThanOne =
  \case
    _:_:_ -> True
    _ -> False

partitionTTExtended :: (w, TT a) -> Either ((w, TT x), String) (a, w, TT a)
partitionTTExtended (s, t) =
  case _ttVal t of
    Fail e -> Left ((s, t & ttVal .~ Fail e), e)
    Val a -> Right (a,s,t)

formatList :: forall x z . Show x
  => POpts
  -> [((Int, x), z)]
  -> String
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showAImpl opts DLite ", a=" a <> ")")

-- | pretty print a tree
toNodeString :: POpts
             -> PE
             -> String
toNodeString opts bpe =
  if hasNoTree opts
  then errorInProgram $ "shouldnt be calling this if we are dropping details: toNodeString " <> show (oDebug opts) <> " " <> show bpe
  else colorValP Long opts (_peValP bpe) <> " " <> _peString bpe

-- | returns True if there the debug option supports a display in tree format
hasNoTree :: POpts -> Bool
hasNoTree opts =
  case oDebug opts of
    DZero -> True
    DLite -> True
    DNormal -> False
    DVerbose -> False

-- | render 'ValP' value with colors
colorValP ::
     Long
  -> POpts
  -> ValP
  -> String
colorValP long o bp =
  case bp of
    FailP e -> case long of
                 Long -> "[" <> f "Error" <> nullSpace e <> "]"
                 Short -> f "Failed"
    FalseP -> f "False"
    TrueP -> f "True"
    ValP -> f "P"
  where f = colorMe o bp

-- | flag for showing long or short output
data Long = Long | Short deriving (Show, Eq)

-- | render 'Val' value with colors
colorValLite :: Show a
    => POpts
    -> (Val a, ValP)
    -> String
colorValLite o (bt,bp') =
  let f = colorMe o bp
      bp = validateValP bp' bt
  in case bt of
       Fail e -> f "Error" <> " " <> e
       Val a -> case bp of
                  FalseP -> f "False"
                  TrueP -> f "True"
                  ValP -> f "Present" <> " " <> show a
                  FailP {} -> errorInProgram $ "colorValLite: unexpected FailP " ++ show (bt,bp)

-- | render coloured output for boolean values
colorValBool ::
      POpts
   -> Val Bool
   -> String
colorValBool o r =
  let f = colorMe o (val2PBool r)
  in case r of
      Fail e -> f "Fail" <> " " <> e
      Val False -> f "False"
      Val True -> f "True"

-- | colors the result of the predicate based on the current color palette
colorMe ::
     POpts
  -> ValP
  -> String
  -> String
colorMe o b s =
  let (_, f) | oNoColor o = nocolor
             | otherwise = oColor o
  in coerce f b s

-- | display tree
prtTreePure ::
     POpts
  -> Tree PE
  -> String
prtTreePure opts t
  | hasNoTree opts = colorValP Long opts (t ^. root . peValP)
  | otherwise = showTreeImpl opts $ fmap (toNodeString opts) t

showTreeImpl :: POpts
         -> Tree String
         -> String
showTreeImpl o =
  case oDisp o of
    Unicode -> drawTreeU
    Ansi -> Safe.initSafe . drawTree -- to drop the last newline else we have to make sure that everywhere else has that newline

-- | extract message part from tree
topMessage :: TT a -> String
topMessage pp =
  let s = _ttString pp
  in unlessNull s $ "(" <> s <> ")"

-- | render numbered tree
prefixNumberToTT :: ((Int, x), TT a) -> TT a
prefixNumberToTT ((i, _), t) = prefixMsg ("i=" <> show i <> ": ") t

-- | prefix text in front of ttString
prefixMsg :: String -> TT a -> TT a
prefixMsg msg = ttString %~ (msg <>)

-- | a typeclass for choosing which monad to run in
--
-- >>> hasIO @IO
-- True
--
-- >>> hasIO @Identity
-- False
--

class Monad m => MonadEval m where
  runIO :: IO a -> m (Maybe a)
  catchit :: a -> m (Either String a)
  catchitNF :: NFData a => a -> m (Either String a)
  liftEval :: m a -> IO a
  hasIO :: Bool
  hasIO = False

-- | 'Identity' instance for evaluating the expression
instance MonadEval Identity where
  runIO _ = Identity Nothing
  catchit = catchitIdentityUnsafe
  catchitNF = catchitNFIdentityUnsafe
  liftEval = return . runIdentity


{-# NOINLINE catchitIdentityUnsafe #-}
catchitIdentityUnsafe :: a -> Identity (Either String a)
catchitIdentityUnsafe v = Identity $ unsafePerformIO $ catchit @IO v

{-# NOINLINE catchitNFIdentityUnsafe #-}
catchitNFIdentityUnsafe :: NFData a => a -> Identity (Either String a)
catchitNFIdentityUnsafe v = Identity $ unsafePerformIO $ catchitNF @IO v


-- | 'IO' instance for evaluating the expression
instance MonadEval IO where
  runIO ioa = Just <$> ioa
  catchit v = E.evaluate (Right $! v) `E.catch` (\(E.SomeException e) -> pure $ Left ("IO e=" <> show e))
  catchitNF v = E.evaluate (Right $!! v) `E.catch` (\(E.SomeException e) -> pure $ Left ("IO e=" <> show e))
  liftEval = id
  hasIO = True

-- composite types are used instead of type synonyms as showT (typeRep) unrolls the definition
-- eg sqlhandler.encode/decode and parsejson* etc
-- | Display options
data Opt =
    OEmpty                -- ^ mempty
  | OWidth !Nat           -- ^ set display width
  | OMsg !Symbol          -- ^ set text to add context to a failure message for refined types
  | ORecursion !Nat       -- ^ set recursion limit eg for regex
  | ORecursionLarge !Nat  -- ^ set recursion limit for large fields
  | OLarge !Bool          -- ^ use large recursion
  | OOther                -- ^ set effects for messages
     !Bool    -- ^ set underline
     !Color   -- ^ set foreground color
     !Color   -- ^ set background color
  | !Opt :# !Opt        -- ^ mappend
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
  | OA                    -- ^ composite: ansi + colors
  | OAB                   -- ^ composite: ansi + colors + background
  | OAN                   -- ^ composite: ansi + no colors
  | OAV                   -- ^ composite: ansi + colors + verbose
  | OANV                  -- ^ composite: ansi + no colors + verbose
  | OU                    -- ^ composite: unicode + colors
  | OUB                   -- ^ composite: unicode + colors + background
  | OUN                   -- ^ composite: unicode + no colors
  | OUV                   -- ^ composite: unicode + colors + verbose
  | OUNV                  -- ^ composite: unicode + no colors + verbose

infixr 6 :#
-- | extract options from the typelevel
class OptC (k :: Opt) where
   getOptC :: HOpts Last
instance KnownNat n => OptC ('OWidth n) where
   getOptC = setWidth (nat @n)
instance KnownSymbol s => OptC ('OMsg s) where
   getOptC = setMessage (symb @s)
instance KnownNat n => OptC ('ORecursion n) where
   getOptC = setRecursion (nat @n)
instance KnownNat n => OptC ('ORecursionLarge n) where
   getOptC = setRecursionLarge (nat @n)
instance GetBool b => OptC ('OLarge b) where
   getOptC = setLarge (getBool @b)
instance ( GetBool b
         , GetColor c1
         , GetColor c2
         ) => OptC ('OOther b c1 c2) where
   getOptC = setOther (getBool @b) (getColor @c1) (getColor @c2)
instance OptC 'OEmpty where
   getOptC = mempty
instance ( OptC a
         , OptC b
         ) => OptC (a ':# b) where
   getOptC = getOptC @a <> getOptC @b
instance ( KnownSymbol s
         , GetColor c1
         , GetColor c2
         , GetColor c3
         , GetColor c4
         , GetColor c5
         , GetColor c6
         , GetColor c7
         , GetColor c8)
  => OptC ('OColor s c1 c2 c3 c4 c5 c6 c7 c8) where
     getOptC = setCreateColor
        (symb @s)
        (getColor @c1)
        (getColor @c2)
        (getColor @c3)
        (getColor @c4)
        (getColor @c5)
        (getColor @c6)
        (getColor @c7)
        (getColor @c8)
instance OptC 'OColorOn where
   getOptC = setNoColor False
instance OptC 'OColorOff where
   getOptC = setNoColor True
instance OptC 'OAnsi where
   getOptC = setDisp Ansi
instance OptC 'OUnicode where
   getOptC = setDisp Unicode
instance OptC 'OZero where
   getOptC = setDebug DZero
instance OptC 'OLite where
   getOptC = setDebug DLite
instance OptC 'ONormal where
   getOptC = setDebug DNormal
instance OptC 'OVerbose where
   getOptC = setDebug DVerbose
instance OptC 'OZ where
   getOptC = setDisp Ansi <> setNoColor True <> setDebug DZero
instance OptC 'OL where
   getOptC = setDisp Ansi <> setNoColor True <> setDebug DLite <> setWidth 200
instance OptC 'OA where
   getOptC = setDisp Ansi <> getOptC @Color5 <> setDebug DNormal <> getOptC @Other2 <> setWidth 100
instance OptC 'OAB where
   getOptC = setDisp Ansi <> getOptC @Color1 <> setDebug DNormal <> getOptC @Other1 <> setWidth 100
instance OptC 'OAN where
   getOptC = setDisp Ansi <> setNoColor True <> setDebug DNormal <> setWidth 100
instance OptC 'OAV where
   getOptC = getOptC @('OA ':# 'OVerbose ':# 'OWidth 200)
instance OptC 'OANV where
   getOptC = setDisp Ansi <> setNoColor True <> setDebug DVerbose <> setWidth 200
instance OptC 'OU where
   getOptC = getOptC @('OA ':# 'OUnicode)
instance OptC 'OUB where
   getOptC = getOptC @('OAB ':# 'OUnicode)
instance OptC 'OUN where
   getOptC = getOptC @('OAN ':# 'OUnicode)
instance OptC 'OUV where
   getOptC = getOptC @('OAV ':# 'OUnicode)
instance OptC 'OUNV where
   getOptC = getOptC @('OANV ':# 'OUnicode)

-- option synonyms to save a keystroke
-- | debugging: no output
type OZ = 'OZ     -- 'OAnsi ':# 'OColorOff ':# 'OZero
-- | debugging: one line of summarised output without colors or effects
type OL = 'OL     -- 'OAnsi ':# 'OColorOff ':# 'OLite ':# 'OWidth 200
-- | ascii output with colours
type OA = 'OA     -- 'OAnsi ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
-- | ascii output with colours and bold background
type OAB = 'OAB   -- 'OAnsi ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
-- | ascii output without colours
type OAN = 'OAN   -- 'OAnsi ':# 'OColorOff ':# 'ONormal ':# 'OWidth 100
-- | ascii output without colours
type OAV = 'OAV   -- 'OAnsi ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200
-- | ascii output without colours
type OANV = 'OANV -- 'OAnsi ':# 'OColorOff ':# 'OVerbose ':# 'OWidth 200
-- | unicode output without colours
type OU = 'OU     -- 'OUnicode ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
-- | unicode output with colors and a bold background
type OUB = 'OUB   -- 'OUnicode ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
-- | unicode output without colours
type OUN = 'OUN   -- 'OUnicode ':# 'OColorOff ':# 'OWidth 200
-- | unicode output verbose with colours
type OUV = 'OUV   -- 'OUnicode ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200
-- | unicode output without colours and verbose
type OUNV = 'OUNV -- 'OUnicode ':# 'OColorOff ':# 'OVerbose ':# 'OWidth 200

-- | convert typelevel options to 'POpts'
--
-- >>> (oDisp &&& fst . oColor &&& oWidth) (getOpt @(OA ':# OU ':# OA ':# 'OWidth 321 ':# Color4 ':# 'OMsg "test message"))
-- (Ansi,("color4",321))
--
-- >>> oMsg (getOpt @('OMsg "abc" ':# 'OMsg "def"))
-- ["abc","def"]
--
-- >>> oOther (getOpt @('OOther 'False 'Red 'White ':# 'OOther 'True 'Red 'Black))
-- (True,Red,Black)
--
-- >>> a = show (getOpt @('OEmpty ':# OU))
-- >>> b = show (getOpt @(OU ':# 'OEmpty));
-- >>> c = show (getOpt @OU)
-- >>> a==b && b==c
-- True
--
getOpt :: forall o . OptC o => POpts
getOpt = reifyOpts (getOptC @o)

-- | deal with possible recursion on a list
chkSize :: Foldable t
   => POpts
   -> String
   -> t a
   -> [Tree PE]
   -> Either (TT x) (Int,[a])
chkSize opts msg0 xs hhs =
  let mx = getMaxRecursionValue opts
  in case splitAt mx (toList xs) of
    (zs,[]) -> Right (length zs,zs)
    (_,_:_) -> Left $ mkNode opts (Fail (msg0 <> " list size exceeded")) ("max is " ++ show mx) hhs

-- | deal with possible recursion on two lists
chkSize2 :: (Foldable t, Foldable u)
   => POpts
   -> String
   -> t a
   -> u b
   -> [Tree PE]
   -> Either (TT x) ((Int,[a]),(Int,[b]))
chkSize2 opts msg0 xs ys hhs =
  (,) <$> chkSize opts msg0 xs hhs <*> chkSize opts msg0 ys hhs

-- | get the current recursion value depending on the oLarge flag
getMaxRecursionValue :: POpts -> Int
getMaxRecursionValue = ifM oLarge oRecursionLarge oRecursion

-- | pretty print a message
formatOMsg :: POpts -> String -> String
formatOMsg o suffix =
  case oMsg o of
    [] -> mempty
    s@(_:_) -> intercalate " | " (map (setOtherEffects o) s) <> suffix

-- | override options for 'DZero' so we dont lose error information: especially if we want to extract the topMessage and use it for Fail case
zeroToLite :: POpts -> POpts
zeroToLite opts =
  case oDebug opts of
    DZero -> opts { oDebug = DLite }
    _ -> opts

-- | render a string for messages using optional color and underline
setOtherEffects :: POpts -> String -> String
setOtherEffects o =
  if oNoColor o then id
  else case coerce (oOther o) of
         (False, Default, Default) -> id
         (b, c1, c2) -> (if b then C.style C.Underline else id) . C.color c1 . C.bgColor c2

-- | mconcat 'Opt' options at the type level
--
-- >>> x = getOpt @(OptT '[ 'OMsg "test", 'ORecursion 123, OU, OL, 'OMsg "field2"])
-- >>> oMsg x
-- ["test","field2"]
-- >>> oRecursion x
-- 123
--
type family OptT (xs :: [Opt]) where
  OptT '[] = 'OEmpty
  OptT (x ': xs) = x ':# OptT xs

-- | message to display when the length of a foldable is exceeded
badLength :: Int
          -> Int
          -> String
badLength asLen n = ":invalid length(" <> show asLen <> ") expected " ++ show n

-- | render the output from TT depending on the debug options
prtTree :: Show x => POpts -> TT x -> String
prtTree opts tt =
  case oDebug opts of
     DZero -> ""

     DLite ->
           formatOMsg opts " >>> "
           <> colorValLite opts ((_ttVal &&& _ttValP) tt)
           <> " "
           <> topMessage tt

     _ -> formatOMsg opts ""
          <> prtTreePure opts (hh tt)

-- | return a singleton list of Tree if in verbose mode
verboseList :: POpts -> TT a -> [Tree PE]
verboseList o tt
  | isVerbose o = [hh tt]
  | otherwise = []

-- | iso for 'Val'
--
-- >>> Val 123 ^. _ValEither
-- Right 123
--
-- >>> Val 123 & _ValEither %~ right' (show . succ)
-- Val "124"
--
-- >>> Fail "abc" & _ValEither %~ ((<>"def") +++ (show . succ))
-- Fail "abcdef"
--
-- >>> Right 1.2 & from _ValEither %~ fmap (show . (*10))
-- Right "12.0"
--
-- >>> Val True ^. _ValEither
-- Right True
--
-- >>> Fail "abc" ^. _ValEither
-- Left "abc"
--
-- >>> Left "abc" ^. from _ValEither
-- Fail "abc"
--
-- >>> _ValEither # Right False
-- Val False
--
-- >>> [Just (Val 'x')] ^. mapping (mapping _ValEither)
-- [Just (Right 'x')]
--
-- >>> Just (Fail "abcd") ^. mapping _ValEither
-- Just (Left "abcd")
--
_ValEither :: Iso (Val a) (Val b) (Either String a) (Either String b)
_ValEither = iso fw bw
  where fw = \case
               Val a -> Right a
               Fail e -> Left e
        bw = either Fail Val

-- | converts from a typed 'Val' to the untyped 'ValP'
--
-- >>> val2P (Val True)
-- ValP
--
-- >>> val2P (Val 123)
-- ValP
--
-- >>> val2P (Fail "abc")
-- FailP "abc"
--
val2P :: Val a -> ValP
val2P =
  \case
    Fail e -> FailP e
    Val {} -> ValP

-- | converts from a typed 'Val' Bool to the untyped 'ValP'
--
-- >>> val2PBool (Val True)
-- TrueP
--
-- >>> val2PBool (Val False)
-- FalseP
--
-- >>> val2PBool (Fail "abc")
-- FailP "abc"
--
val2PBool :: a ~ Bool => Val a -> ValP
val2PBool =
  \case
    Fail e -> FailP e
    Val True -> TrueP
    Val False -> FalseP

-- | lens that keeps 'ValP' in sync with 'Val' for TT Bool
--
-- >>> (TT ValP (Val True) "xxx" [] & ttValBool %~ \b -> fmap not b) == TT FalseP (Val False) "xxx" []
-- True
--
-- >>> (TT ValP (Val True) "xxx" [] & ttValBool .~ Fail "abc") == TT (FailP "abc") (Fail "abc") "xxx" []
-- True
--
-- >>> (TT ValP (Val True) "xxx" [] & ttValBool %~ id) == TT TrueP (Val True) "xxx" []
-- True
--
-- >>> (TT FalseP (Val True) "xxx" [] & ttValBool %~ id) == TT TrueP (Val True) "xxx" []
-- True
--
ttValBool :: a ~ Bool => Lens' (TT a) (Val Bool)
ttValBool afb tt = (\b -> tt { _ttValP = val2PBool b, _ttVal = b }) <$> afb (_ttVal tt)

-- | lens from 'TT' to 'Val' that also keeps 'ValP' in sync with 'Val'
--
-- >>> (TT FalseP (Val True) "xxx" [] & ttVal %~ id) == TT ValP (Val True) "xxx" []
-- True
--
-- >>> (TT FalseP (Val 123) "xxx" [] & ttVal .~ Fail "aa") == TT (FailP "aa") (Fail "aa") "xxx" []
-- True
--
-- >>> (TT (FailP "sdf") (Val 123) "xxx" [] & ttVal %~ fmap show) == TT ValP (Val "123") "xxx" []
-- True
--
ttVal :: Lens (TT a) (TT b) (Val a) (Val b)
ttVal afb tt = (\b -> tt { _ttValP = val2P b, _ttVal = b }) <$> afb (_ttVal tt)

