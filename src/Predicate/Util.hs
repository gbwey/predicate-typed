{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# LANGUAGE DerivingStrategies #-}
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
{- |
     Utility methods for Predicate / methods for displaying the evaluation tree
-}
module Predicate.Util (
  -- ** TT
    TT(..)
  , ttBoolT
  , ttBoolP
  , ttString
  , ttForest
  , boolT2P
  , topMessage
  , hasNoTree

 -- ** BoolT
  , BoolT(..)
  , _FailT
  , _PresentT
  , _BoolT
  , _TrueT
  , _FalseT
  , _BoolTIso

 -- ** PE
  , PE(..)
  , pBool
  , pString

 -- ** BoolP
  , BoolP(..)
  , _FailP
  , _TrueP
  , _FalseP
  , _PresentP

 -- ** create tree
  , mkNode
  , mkNode'
  , mkNodeCopy
  , mkNodeB

 -- ** tree manipulation
  , getValAndPE
  , getValLRFromTT
  , fromTT
  , getValueLR
  , fixLite
  , fixit
  , prefixMsg
  , splitAndAlign
  , verboseList
  , fixEmptyNode
  , fixTTBoolP

 -- ** options
  , POpts
  , Debug(..)
  , Disp(..)
  , isVerbose
  , colorBoolTBool
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
  , nullSpace
  , nullIf

 -- ** printing methods
  , prtTreePure
  , formatOMsg
  , prtTree

 -- ** miscellaneous
  , compileRegex
  , Holder
  , hh
  , removeAnsi
  , MonadEval(..)
  , chkSize
  , chkSize2
  , pureTryTest
  , pureTryTestPred
  , unlessNull
  , unlessNullM
  , badLength
  , showIndex

  , module Predicate.Misc

  ) where
import Predicate.Misc
import GHC.TypeLits (Symbol, Nat, KnownSymbol, KnownNat)
import Control.Lens
import Control.Arrow
import Data.List (intercalate, unfoldr)
import Data.Tree (drawTree, Forest, Tree(Node))
import Data.Tree.Lens (root)
import System.Console.Pretty
import qualified Text.Regex.PCRE.Heavy as RH
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
import Data.Char (isSpace)
import qualified Safe (initSafe, fromJustNote)
import Control.Monad (ap)
import Data.Bool (bool)
import GHC.Generics (Generic, Generic1)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | contains the untyped result from evaluating the expression tree
data BoolP =
    FailP !String -- ^ fails the entire evaluation
  | FalseP       -- ^ False predicate
  | TrueP        -- ^ True predicate
  | PresentP     -- ^ Any value
  deriving stock (Show, Ord, Eq, Read, Generic)

makePrisms ''BoolP

-- | untyped evaluation tree for final display
data PE = PE { _pBool :: !BoolP -- ^ holds the result of running the predicate
             , _pString :: !String -- ^ optional strings to include in the results
             } deriving stock (Show, Read, Eq, Generic)

makeLenses ''PE

-- | contains the typed result from evaluating the expression tree
data BoolT a = FailT !String | PresentT !a
  deriving stock (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Generic, Generic1)

makePrisms ''BoolT

instance Applicative BoolT where
  pure = PresentT
  (<*>) = ap

instance Monad BoolT where
  return = pure
  PresentT a >>= amb = amb a
  FailT s >>= _ = FailT s

-- | semigroup instance for 'BoolT'
--
-- >>> PresentT 123 <> (PresentT 456 <> PresentT 789) == (PresentT 123 <> PresentT 456) <> PresentT 789
-- True
--
-- >>> PresentT True <> PresentT False
-- PresentT False
--
-- >>> PresentT True <> PresentT True
-- PresentT True
--
-- >>> FailT "abc" <> (PresentT True <> PresentT False) <> FailT "def"
-- FailT "abcdef"
--
-- >>> (FailT "abc" <> PresentT True) <> (PresentT False <> FailT "def")
-- FailT "abcdef"
--
-- >>> PresentT False <> (PresentT True <> PresentT False) == (PresentT False <> PresentT True) <> PresentT False
-- True
--
instance Semigroup (BoolT a) where
   FailT s <> FailT s1 = FailT (s <> s1)
   FailT s <> _ = FailT s
   _ <> FailT s = FailT s
   PresentT _a <> PresentT b = PresentT b

-- | 'Read' instance for BoolT
--
-- >>> reads @(BoolT Int) "PresentT 123"
-- [(PresentT 123,"")]
--
-- >>> reads @(BoolT Bool) "PresentT False abc"
-- [(PresentT False," abc")]
--
-- >>> reads @(BoolT Bool) "FailT \"some error message\""
-- [(FailT "some error message","")]
--
-- >>> reads @(BoolT Double) "FailT \"some error message\""
-- [(FailT "some error message","")]
--

-- | evaluation tree for predicates
data TT a = TT { _ttBoolT :: !(BoolT a)  -- ^ the value at this root node
               , _ttString :: !String  -- ^ detailed information eg input and output and text
               , _ttForest :: !(Forest PE) -- ^ the child nodes
               , _ttBoolP :: !BoolP -- ^ display value
               } deriving stock (Functor, Read, Show, Eq, Foldable, Traversable, Generic, Generic1)

makeLenses ''TT

instance Applicative TT where
  pure a = TT (pure a) "" [] PresentP
  (<*>) = ap

instance Monad TT where
  return = pure
  TT (PresentT a) y z _p >>= amb =
    let TT w _y1 z1 p1 = amb a
    in TT w (y++ nullIf " | " _y1) (z <> z1) p1
  TT (FailT s) y z _ >>= _ = TT (FailT s) y z (FailP s)

-- | a lens from typed 'BoolT' to the untyped 'BoolP'
boolT2P :: Lens' (BoolT a) BoolP
boolT2P afb = \case
  FailT e -> FailT e <$ afb (FailP e)
  PresentT a -> PresentT a <$ afb PresentP

-- | creates a Node for the evaluation tree
mkNodeCopy :: POpts
       -> TT a
       -> String
       -> [Holder]
       -> TT a
mkNodeCopy opts tt ss hs = mkNode' opts (_ttBoolT tt) ss hs (_ttBoolP tt)


-- | creates a Node for the evaluation tree
mkNode :: POpts
       -> BoolT a
       -> String
       -> [Holder]
       -> TT a
mkNode opts bt ss hs = mkNode' opts bt ss hs (bt ^. boolT2P)

-- | creates a Node for the evaluation tree
mkNode' :: POpts
       -> BoolT a
       -> String
       -> [Holder]
       -> BoolP
       -> TT a
mkNode' opts bt ss hs bp' =
  let bp = validateBoolP bt bp'
  in case oDebug opts of
      DZero -> TT bt [] [] bp
      DLite ->
      -- keeps the last string so we can use the root to give more details on failure (especially for Refined* types)
      -- also holds onto any failures
          let zs = filter (\(Holder x) -> has (ttBoolT . _FailT) x) hs
          in TT bt ss (map fromTTH zs) bp
      _ -> TT bt ss (map fromTTH hs) bp

validateBoolP :: BoolT a -> BoolP -> BoolP
validateBoolP bt bp =
  case bt of
    PresentT _a -> case bp of
                     FailP e -> errorInProgram $ "validateBoolP: found FailP for PresentT in BoolT e=" ++ e
                     _ -> bp
    FailT e -> case bp of
                FailP e1 | e==e1 -> bp
                         | otherwise -> errorInProgram $ "validateBoolP: found FailT but message mismatch in BoolP " ++ show (e,e1)
                _ -> errorInProgram $ "validateBoolP: found " ++ show bp ++ " expected FailP e=" ++ e

fixTTBoolP :: TT Bool -> TT Bool
fixTTBoolP tt = tt { _ttBoolP = getBoolP (_ttBoolT tt) }

getBoolP :: BoolT Bool -> BoolP
getBoolP bt =
  case bt of
    PresentT True -> TrueP
    PresentT False -> FalseP
    FailT e -> FailP e

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts
        -> Bool
        -> String
        -> [Holder]
        -> TT Bool
mkNodeB opts b s tt =
  mkNode' opts (PresentT b) s tt (bool FalseP TrueP b)

getValAndPE :: TT a -> (Either String a, Tree PE)
getValAndPE tt = (getValLRFromTT tt, fromTT tt)

getValLRFromTT :: TT a -> Either String a
getValLRFromTT = getValLR  . _ttBoolT

-- | get the value from BoolT or fail
getValLR :: BoolT a -> Either String a
getValLR = \case
    FailT e -> Left e
    PresentT a -> Right a

-- | converts a typed tree to an untyped tree for display
fromTT :: TT a -> Tree PE
fromTT (TT bt ss tt bp) = Node (PE (validateBoolP bt bp) ss) tt

-- | a monomorphic container of trees
data Holder = forall w . Holder !(TT w)

-- | converts a typed tree into an untyped one
fromTTH :: Holder -> Tree PE
fromTTH (Holder x) = fromTT x

-- | convenience method to wrap a typed tree
hh :: TT w -> Holder
hh = Holder

-- | decorate the tree with more detail when there are errors
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

-- | elide the 'Identity' wrapper so it acts like a normal adt
type family HKD f a where
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
        , oRecursion :: !(HKD f Int) -- ^ max recursion
        , oOther :: !(HKD f (Bool, SColor, SColor)) -- ^ other message effects
        , oNoColor :: !(HKD f Bool) -- ^ no colors
        }

-- | the color palette for displaying the expression tree
newtype PColor = PColor (BoolP -> String -> String)
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

-- | combine options ala monoid
reifyOpts :: HOpts Last -> HOpts Identity
reifyOpts h =
  HOpts (fromMaybe (oWidth defOpts) (coerce (oWidth h)))
        (fromMaybe (oDebug defOpts) (coerce (oDebug h)))
        (fromMaybe (oDisp defOpts) (coerce (oDisp h)))
        (if fromMaybe (oNoColor defOpts) (coerce (oNoColor h)) then nocolor
         else fromMaybe (oColor defOpts) (coerce (oColor h)))
        (oMsg defOpts <> oMsg h)
        (fromMaybe (oRecursion defOpts) (coerce (oRecursion h)))
        (if fromMaybe (oNoColor defOpts) (coerce (oNoColor h)) then otherDef
         else fromMaybe (oOther defOpts) (coerce (oOther h)))
        (fromMaybe (oNoColor defOpts) (coerce (oNoColor h)))

-- | set maximum display width of expressions
setWidth :: Int -> HOpts Last
setWidth i = mempty { oWidth = pure i }

-- | set title message for the display tree
setMessage :: String -> HOpts Last
setMessage s = mempty { oMsg = pure s }

-- | set maximum recursion eg when running regex
setRecursion :: Int -> HOpts Last
setRecursion i = mempty { oRecursion = pure i }

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
       FailP {} -> color c1 . bgColor c2
       FalseP -> color c3 . bgColor c4
       TrueP -> color c5 . bgColor c6
       PresentP -> color c7 . bgColor c8
  in mempty { oColor = pure (s,PColor pc) }

-- | set debug mode
setDebug :: Debug -> HOpts Last
setDebug d =
  mempty { oDebug = pure d }

-- | monoid opts
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
    , oOther = otherDef
    , oNoColor = False
    }

-- | default title message color and boundaries between multipart refine messages
otherDef :: (Bool, SColor, SColor)
otherDef = coerce (True, Default, Default)

nocolor, colorDef :: (String, PColor)
nocolor = ("nocolor", PColor $ flip const)
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

-- | color palettes
type Color1 = 'OColor "color1" 'Default 'Blue 'Default 'Red 'Black 'Cyan 'Black 'Yellow
type Color2 = 'OColor "color2" 'Default 'Magenta 'Default 'Red 'Black 'White 'Black 'Yellow
type Color3 = 'OColor "color3" 'Default 'Blue 'Red 'Default 'White 'Default 'Black 'Yellow
type Color4 = 'OColor "color4" 'Default 'Red 'Red 'Default 'Green 'Default 'Black 'Yellow
type Color5 = 'OColor "color5" 'Blue 'Default 'Red 'Default 'Cyan 'Default 'Yellow 'Default

type Other1 = 'OOther 'True 'Yellow 'Default
type Other2 = 'OOther 'True 'Default 'Default

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
      <> litVerbose opts (" | " <> take 100 fmt) as

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
  if oDebug o >= i || oDebug o == DLite then take 100 s <> litL o a
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
showAImpl o i s a = showLitImpl o i (take 100 s) (show a)

showL :: Show a
  => POpts
  -> a
  -> String
showL o = litL o . show

litL :: POpts -> String -> String
litL = litL' . oWidth

litL' :: Int -> String -> String
litL' i s =
  let z = take i s
  in z ++ if length z >= i then "..." else ""

litBL :: POpts -> BL8.ByteString -> String
litBL o s =
  let i = oWidth o
  in litL' i (BL8.unpack (BL8.take (fromIntegral i) s))

litBS :: POpts -> BS8.ByteString -> String
litBS o s =
  let i = oWidth o
  in litL' i (BS8.unpack (BS8.take i s))

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
  case _ttBoolT t of
    FailT e -> Left ((s, t & ttBoolT .~ FailT e), e)
    PresentT a -> Right (a,s,t)

formatList :: forall x z . Show x
  => POpts
  -> [((Int, x), z)]
  -> String
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showAImpl opts DLite ", a=" a <> ")")

-- (_BoolT %~ length) <$> pz @Pairs [1..4]
-- (over _BoolT length) <$> pz @Pairs [1..4]
-- fmapB length $ pz @Pairs [1..4]

-- | BoolT prism
--
-- >>> _BoolT # 123
-- PresentT 123
--
-- >>> PresentT 123 ^? _BoolT
-- Just 123
--
-- >>> FailT "abc" ^? _BoolT
-- Nothing
--
-- >>> PresentT 1 & _BoolT .~ True
-- PresentT True
--
-- >>> PresentT False & _BoolT %~ not
-- PresentT True
--
-- >>> FailT "asdF" & _BoolT .~ True
-- FailT "asdF"
--
_BoolT :: Prism (BoolT a) (BoolT b) a b
_BoolT = prism PresentT
         $ \case
              PresentT a -> Right a
              FailT e -> Left (FailT e)

-- | pretty print a tree
toNodeString :: POpts
             -> PE
             -> String
toNodeString opts bpe =
  if hasNoTree opts
  then errorInProgram $ "shouldnt be calling this if we are dropping details: toNodeString " <> show (oDebug opts) <> " " <> show bpe
  else colorBoolP opts (_pBool bpe) <> _pString bpe

hasNoTree :: POpts -> Bool
hasNoTree opts =
  case oDebug opts of
    DZero -> True
    DLite -> True
    DNormal -> False
    DVerbose -> False

nullSpace :: String -> String
nullSpace = nullIf " "

nullIf :: String -> String -> String
nullIf s t
  | all isSpace t = ""
  | otherwise = s <> t

-- | render the 'BoolP' value with colors
colorBoolP ::
     POpts
  -> BoolP
  -> String
colorBoolP o b =
  case b of
    FailP e -> "[" <> f "Error" <> nullSpace e <> "] "
    PresentP -> f "P "
    TrueP -> f "True "
    FalseP -> f "False "
  where f = colorMe o b

-- | render the 'BoolT' value with colors
colorBoolTLite :: Show a
    => POpts
    -> BoolT a
    -> BoolP
    -> String
colorBoolTLite o bt bp =
  let f = colorMe o bp
  in case bt of
      FailT e -> f "Error " <> e
      PresentT x -> case bp of
                      PresentP -> f "Present " <> show x
                      TrueP -> f "True"
                      FalseP -> f "False"
                      FailP _ -> errorInProgram $ "colorBoolTLite: unexpected FailP " ++ show (bt,bp)

colorBoolTBool ::
      POpts
   -> BoolT Bool
   -> String
colorBoolTBool o r =
  uncurry (colorMe o) $
  case r of
      FailT e -> (FailP e, "FailT " <> e)
      PresentT True -> (TrueP, "TrueT")
      PresentT False -> (FalseP, "FalseT")

-- | colors the result of the predicate based on the current color palette
colorMe ::
     POpts
  -> BoolP
  -> String
  -> String
colorMe o b s =
  let (_, f) | oNoColor o = nocolor
             | otherwise = oColor o
  in coerce f b s

-- | override PresentP case if there is no tree ie lite or zero mode
fixLite :: forall a . Show a
   => POpts
   -> a
   -> Tree PE
   -> String
fixLite opts a t
  | hasNoTree opts =
      let r = case t ^. root . pBool of
                PresentP -> colorMe opts PresentP "Present " <> show a
                bp -> colorBoolP opts bp
      in r <> "\n"
  | otherwise = prtTreePure opts t

-- | display tree
prtTreePure ::
     POpts
  -> Tree PE
  -> String
prtTreePure opts t
  | hasNoTree opts = colorBoolP opts (t ^. root . pBool)
  | otherwise = showImpl opts $ fmap (toNodeString opts) t

-- | extract message part from tree
topMessage :: TT a -> String
topMessage pp =
  let s = _ttString pp
  in unlessNull s $ "(" <> s <> ")"

showImpl :: POpts
         -> Tree String
         -> String
showImpl o =
  case oDisp o of
    Unicode -> drawTreeU
    Ansi -> Safe.initSafe . drawTree -- to drop the last newline else we have to make sure that everywhere else has that newline: eg fixLite

-- | render numbered tree
fixit :: ((Int, x), TT a) -> TT a
fixit ((i, _), t) = prefixMsg ("i=" <> show i <> ": ") t

-- | prefix text in front of ttString
prefixMsg :: String -> TT a -> TT a
prefixMsg msg = ttString %~ (msg <>)

-- | a typeclass for choosing which monad to run in
class Monad m => MonadEval m where
  runIO :: IO a -> m (Maybe a)
  catchit :: a -> m (Either String a)
  catchitNF :: NFData a => a -> m (Either String a)
  liftEval :: m a -> IO a

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

-- composite types are used instead of type synonyms as showT (typeRep) unrolls the definition
-- eg sqlhandler.encode/decode and parsejson* etc
-- | Display options
data Opt =
    OEmpty                -- ^ mempty
  | OWidth !Nat           -- ^ set display width
  | OMsg !Symbol          -- ^ set text to add context to a failure message for refined types
  | ORecursion !Nat       -- ^ set recursion limit eg for regex
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

-- | option synonyms to save a keystroke
type OZ = 'OZ     -- 'OAnsi ':# 'OColorOff ':# 'OZero
type OL = 'OL     -- 'OAnsi ':# 'OColorOff ':# 'OLite ':# 'OWidth 200
type OA = 'OA     -- 'OAnsi ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
type OAB = 'OAB   -- 'OAnsi ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
type OAN = 'OAN   -- 'OAnsi ':# 'OColorOff ':# 'ONormal ':# 'OWidth 100
type OAV = 'OAV   -- 'OAnsi ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200
type OANV = 'OANV -- 'OAnsi ':# 'OColorOff ':# 'OVerbose ':# 'OWidth 200
type OU = 'OU     -- 'OUnicode ':# Color5 ':# 'ONormal ':# Other2 ':# 'OWidth 100
type OUB = 'OUB   -- 'OUnicode ':# Color1 ':# 'ONormal ':# Other1 ':# 'OWidth 100
type OUN = 'OUN   -- 'OUnicode ':# 'OColorOff ':# 'OWidth 200
type OUV = 'OUV   -- 'OUnicode ':# Color5 ':# 'OVerbose ':# Other2 ':# 'OWidth 200
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
   -> [Holder]
   -> Either (TT x) [a]
chkSize opts msg0 xs hhs =
  let mx = oRecursion opts
  in case splitAt mx (toList xs) of
    (zs,[]) -> Right zs
    (_,_:_) -> Left $ mkNode opts (FailT (msg0 <> " list size exceeded")) ("max is " ++ show mx) hhs

-- | deal with possible recursion on two lists
chkSize2 :: (Foldable t, Foldable u)
   => POpts
   -> String
   -> t a
   -> u b
   -> [Holder]
   -> Either (TT x) ([a],[b])
chkSize2 opts msg0 xs ys hhs =
 (,) <$> chkSize opts msg0 xs hhs <*> chkSize opts msg0 ys hhs

-- | pretty print a message
formatOMsg :: POpts -> String -> String
formatOMsg o suffix =
  case oMsg o of
    [] -> mempty
    s@(_:_) -> intercalate " | " (map (setOtherEffects o) s) <> suffix

-- | override options for 'DZero' so we dont lose error information
subopts :: POpts -> POpts
subopts opts =
  case oDebug opts of
    DZero -> opts { oDebug = DLite }
    _ -> opts

-- | render a string for messages using optional color and underline
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

-- | convenience method for optional display
unlessNull :: (Foldable t, Monoid m) => t a -> m -> m
unlessNull t m | null t = mempty
               | otherwise = m

unlessNullM :: (Foldable t, Applicative m) => t a -> (t a -> m ()) -> m ()
unlessNullM t f
  | null t = pure ()
  | otherwise = f t

-- | message to display when the length of a foldable is exceeded
badLength :: Foldable t
          => t a
          -> Int
          -> String
badLength as n = ":invalid length(" <> show (length as) <> ") expected " ++ show n

prtTree :: Show x => POpts -> TT x -> String
prtTree opts pp =
  case oDebug opts of
     DZero -> ""

     DLite ->
           formatOMsg opts " >>> "
           <> colorBoolTLite opts (pp ^. ttBoolT) (pp ^. ttBoolP)
           <> " "
           <> topMessage pp

     _ -> formatOMsg opts ""
          <> prtTreePure opts (fromTT pp)

showIndex :: (Show i, Num i) => i -> String
showIndex i = show (i+0)

verboseList :: POpts -> TT a -> [Holder]
verboseList o tt
  | isVerbose o = [hh tt]
  | otherwise = []

-- https://github.com/haskell/containers/pull/344
drawTreeU :: Tree String -> String
drawTreeU  = intercalate "\n" . drawU

drawU :: Tree String -> [String]
drawU (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        shift "\x2514\x2500" "  " (drawU t)
    drawSubTrees (t:ts) =
        shift "\x251c\x2500" "\x2502 " (drawU t) ++ drawSubTrees ts

    shift one other = zipWith (++) (one : repeat other)

fixEmptyNode :: String -> TT a -> TT a
fixEmptyNode s = over (ttForest . traverse) (fixEmptyNode' s)

fixEmptyNode' :: String -> Tree PE -> Tree PE
fixEmptyNode' s = go
 where go (Node (PE PresentP "") []) = Node (PE PresentP s) []
       go (Node p xs) = Node p (map go xs)

-- | prism for PresentT True
--
-- >>> PresentT True ^? _TrueT
-- Just ()
--
-- >>> PresentT False ^? _TrueT
-- Nothing
--
_TrueT :: a ~ Bool => Prism' (BoolT a) ()
_TrueT =
  prism' (const (PresentT True)) $ \case
                       PresentT True -> Just ()
                       _ -> Nothing

-- | prism for PresentT False
--
-- >>> PresentT False ^? _FalseT
-- Just ()
--
-- >>> PresentT True ^? _FalseT
-- Nothing
--
_FalseT :: a ~ Bool => Prism' (BoolT a) ()
_FalseT =
  prism' (const (PresentT False)) $ \case
                       PresentT False -> Just ()
                       _ -> Nothing

-- | iso for BoolT
--
-- >>> PresentT False ^. _BoolTIso
-- Right False
--
-- >>> PresentT True ^. _BoolTIso
-- Right True
--
-- >>> FailT "abc" ^. _BoolTIso
-- Left "abc"
--
-- >>> Left "abc" ^. from _BoolTIso
-- FailT "abc"
--
-- >>> Right False ^. from _BoolTIso
-- PresentT False
--
_BoolTIso :: a ~ Bool => Iso' (BoolT a) (Either String Bool)
_BoolTIso = iso fw bw
  where fw = \case
               PresentT True -> Right True
               PresentT False -> Right False
               FailT e -> Left e
        bw = either FailT PresentT

