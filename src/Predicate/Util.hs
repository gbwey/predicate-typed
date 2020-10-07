{-# OPTIONS -Wall #-}
{-# OPTIONS -Wcompat #-}
{-# OPTIONS -Wincomplete-record-updates #-}
{-# OPTIONS -Wincomplete-uni-patterns #-}
{-# OPTIONS -Wredundant-constraints #-}
{-# OPTIONS -Wunused-type-patterns #-}
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
  , ttVal
  , ttValP
  , ttString
  , ttForest
  , _Val2P
  , topMessage
  , hasNoTree

 -- ** Val
  , Val(..)
  , _Fail
  , _Val
  , _ValE
  , _True
  , _False
  , _ValEither
  , _Val2BoolP

 -- ** PE
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
  , mkNodeCopy
  , mkNodeB

 -- ** tree manipulation
  , getValAndPE
  , getValLRFromTT
  , getValueLR
  , fixLite
  , fixit
  , prefixMsg
  , splitAndAlign
  , verboseList
  , fixEmptyNode
  , fixTTValP

 -- ** options
  , POpts
  , Debug(..)
  , Disp(..)
  , Color(..)
  , isVerbose
  , colorValBool
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

 -- ** printing methods
  , prtTreePure
  , formatOMsg
  , prtTree

 -- ** miscellaneous
  , compileRegex
  , hh
  , removeAnsi
  , MonadEval(..)
  , chkSize
  , chkSize2
  , badLength
  , showIndex

  ) where
import Predicate.Misc
import GHC.TypeLits (Symbol, Nat, KnownSymbol, KnownNat)
import Control.Lens
import Control.Arrow (Arrow((&&&)), ArrowChoice(left))
import Data.List (intercalate, unfoldr)
import Data.Tree (drawTree, Forest, Tree(Node))
import Data.Tree.Lens (root)
import System.Console.Pretty (Color(..))
import qualified System.Console.Pretty as C
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
import qualified Safe (initSafe, fromJustNote)
import Control.Monad (ap)
import Data.Bool (bool)
import GHC.Generics (Generic, Generic1)
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators

-- | contains the untyped result from evaluating the expression tree
data ValP =
    FailP !String -- ^ fails the entire evaluation
  | FalseP       -- ^ False predicate
  | TrueP        -- ^ True predicate
  | ValP     -- ^ Any value
  deriving stock (Show, Ord, Eq, Read, Generic)

makePrisms ''ValP

-- | untyped child node for 'TT'
data PE = PE { _peValP :: !ValP -- ^ holds the result of running the predicate
             , _peString :: !String -- ^ optional strings to include in the results
             } deriving stock (Show, Read, Eq, Generic)

makeLenses ''PE

instance Semigroup ValP where
   FailP s <> FailP s1 = FailP (s <> s1)
   FailP s <> _ = FailP s
   _ <> FailP s = FailP s
   _ <> ValP = ValP
   _ <> TrueP = TrueP
   _ <> FalseP = FalseP

instance Monoid ValP where
  mempty = ValP

-- | contains the typed result from evaluating the expression
data Val a = Fail !String | Val !a
  deriving stock (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Generic, Generic1)

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
-- Fail "abcdef"
--
-- >>> (Fail "abc" <> Val True) <> (Val False <> Fail "def")
-- Fail "abcdef"
--
-- >>> Val False <> (Val True <> Val False) == (Val False <> Val True) <> Val False
-- True
--
instance Semigroup (Val a) where
   Fail s <> Fail s1 = Fail (s <> s1)
   Fail s <> _ = Fail s
   _ <> Fail s = Fail s
   Val _a <> Val b = Val b

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

-- | evaluation tree for predicates
data TT a = TT { _ttValP :: !ValP -- ^ display value
               , _ttVal :: !(Val a)  -- ^ the value at this root node
               , _ttString :: !String  -- ^ detailed information eg input and output and text
               , _ttForest :: !(Forest PE) -- ^ the child nodes
               } deriving stock (Functor, Read, Show, Eq, Foldable, Traversable, Generic, Generic1)

makeLenses ''TT

instance Semigroup (TT a) where
   TT bp bt ss ts <> TT bp1 bt1 ss1 ts1 =
     TT (bp <> bp1) (bt <> bt1) (ss <> (if null ss || null ss1 then "" else " | ") <> ss1) (ts <> ts1)

instance Monoid a => Monoid (TT a) where
   mempty = TT mempty mempty mempty mempty

instance Applicative TT where
  pure a = TT ValP (pure a) "" []
  (<*>) = ap

instance Monad TT where
  return = pure
{- yurk
  TT _ (Val a) ss ts >>= amb =
    let TT bp bt ss1 ts1 = amb a
    in TT bp bt (ss <> (if null ss || null ss1 then "" else " | ") <> ss1) (ts <> ts1)
  TT _ (Fail e) ss ts >>= _ = TT (FailP e) (Fail e) ss ts
-}
  z@(TT _ bt ss ts) >>= amb =
     case bt of
       Val a -> amb a & ttString %~ (\ss1 -> ss <> (if null ss || null ss1 then "" else " | ") <> ss1)
                      & ttForest %~ (ts <>)
       Fail e -> z & ttVal .~ Fail e
                   & ttValP .~ FailP e

-- | creates a Node for the evaluation tree
mkNodeCopy :: POpts
       -> TT a
       -> String
       -> [Tree PE]
       -> TT a
mkNodeCopy opts tt = mkNodeImpl opts (_ttValP tt) (_ttVal tt)

-- | creates a Node for the evaluation tree
mkNode :: POpts
       -> Val a
       -> String
       -> [Tree PE]
       -> TT a
mkNode opts bt = mkNodeImpl opts (bt ^. _Val2P) bt

-- | creates a Node for the evaluation tree
mkNodeImpl :: POpts
           -> ValP
           -> Val a
           -> String
           -> [Tree PE]
           -> TT a
mkNodeImpl opts bp' bt ss hs =
  let bp = validateValP bp' bt
  in case oDebug opts of
      DZero -> TT bp bt "" []
      DLite ->
      -- keeps the last string so we can use the root to give more details on failure (especially for Refined* types)
      -- also holds onto any failures
          let zs = filter (has (root . peValP . _FailP)) hs
          in TT bp bt ss zs
      _ -> TT bp bt ss hs

validateValP :: ValP -> Val a -> ValP
validateValP bp bt =
  case bt of
    Val _a -> case bp of
                     FailP e -> errorInProgram $ "validateValP: found FailP for Val in Val e=" ++ e
                     _ -> bp
    Fail e -> case bp of
                FailP e1 | e==e1 -> bp
                         | otherwise -> errorInProgram $ "validateValP: found Fail but message mismatch in ValP " ++ show (e,e1)
                _ -> errorInProgram $ "validateValP: found " ++ show bp ++ " expected FailP e=" ++ e

fixTTValP :: TT Bool -> TT Bool
fixTTValP tt = tt { _ttValP = tt ^. ttVal . _Val2BoolP }

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts
        -> Bool
        -> String
        -> [Tree PE]
        -> TT Bool
mkNodeB opts b = mkNodeImpl opts (bool FalseP TrueP b) (Val b)

getValAndPE :: TT a -> (Either String a, Tree PE)
getValAndPE = getValLRFromTT &&& hh

getValLRFromTT :: TT a -> Either String a
getValLRFromTT = view (ttVal . _ValEither)

-- | converts a typed tree to an untyped tree for display
hh :: TT a -> Tree PE
hh (TT bp bt ss tt) = Node (PE (validateValP bp bt) ss) tt

-- | decorate the tree with more detail when there are errors
getValueLR :: POpts
           -> String
           -> TT a
           -> [Tree PE]
           -> Either (TT x) a
getValueLR opts msg0 tt hs =
  left (\e -> mkNode opts (Fail e) msg0 (hs ++ [hh tt]))
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

-- | combine options ala monoid
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
       FailP {} -> C.color c1 . C.bgColor c2
       FalseP -> C.color c3 . C.bgColor c4
       TrueP -> C.color c5 . C.bgColor c6
       ValP -> C.color c7 . C.bgColor c8
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
  -> [Tree PE]
  -> Either (TT a) RH.Regex
compileRegex opts nm s hhs
  | null s = Left (mkNode opts (Fail "Regex cannot be empty") nm hhs)
  | otherwise =
      let rs = getROpts @rs
          mm = nm <> " " <> show rs
      in flip left (RH.compileM (TE.encodeUtf8 (T.pack s)) (snd rs))
            $ \e -> mkNode opts (Fail "Regex failed to compile") (mm <> ":" <> e) hhs

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
   . map (\xs@(x :| _) -> x <> (if length xs > 1 then "(" <> show (length xs) <> ")" else ""))
   . N.group

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
  else colorValP opts (_peValP bpe) <> _peString bpe

hasNoTree :: POpts -> Bool
hasNoTree opts =
  case oDebug opts of
    DZero -> True
    DLite -> True
    DNormal -> False
    DVerbose -> False

-- | render the 'ValP' value with colors
colorValP ::
     POpts
  -> ValP
  -> String
colorValP o b =
  case b of
    FailP e -> "[" <> f "Error" <> nullSpace e <> "] "
    ValP -> f "P "
    TrueP -> f "True "
    FalseP -> f "False "
  where f = colorMe o b

-- | render the 'Val' value with colors
colorValLite :: Show a
    => POpts
    -> Val a
    -> ValP
    -> String
colorValLite o bt bp =
  let f = colorMe o bp
  in case bt of
      Fail e -> f "Error " <> e
      Val x -> case bp of
                      ValP -> f "Present " <> show x
                      TrueP -> f "True"
                      FalseP -> f "False"
                      FailP _ -> errorInProgram $ "colorValLite: unexpected FailP " ++ show (bt,bp)

colorValBool ::
      POpts
   -> Val Bool
   -> String
colorValBool o r =
  colorMe o (r ^. _Val2BoolP)
  $ case r of
      Fail e -> "Fail " <> e
      Val True -> "TrueT"
      Val False -> "FalseT"

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

-- | override ValP case if there is no tree ie lite or zero mode
fixLite :: forall a . Show a
   => POpts
   -> a
   -> Tree PE
   -> String
fixLite opts a t
  | hasNoTree opts =
      let r = case t ^. root . peValP of
                ValP -> colorMe opts ValP "Present " <> show a
                bp -> colorValP opts bp
      in r <> "\n"
  | otherwise = prtTreePure opts t

-- | display tree
prtTreePure ::
     POpts
  -> Tree PE
  -> String
prtTreePure opts t
  | hasNoTree opts = colorValP opts (t ^. root . peValP)
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
   -> [Tree PE]
   -> Either (TT x) [a]
chkSize opts msg0 xs hhs =
  let mx = oRecursion opts
  in case splitAt mx (toList xs) of
    (zs,[]) -> Right zs
    (_,_:_) -> Left $ mkNode opts (Fail (msg0 <> " list size exceeded")) ("max is " ++ show mx) hhs

-- | deal with possible recursion on two lists
chkSize2 :: (Foldable t, Foldable u)
   => POpts
   -> String
   -> t a
   -> u b
   -> [Tree PE]
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
           <> colorValLite opts (pp ^. ttVal) (pp ^. ttValP)
           <> " "
           <> topMessage pp

     _ -> formatOMsg opts ""
          <> prtTreePure opts (hh pp)

showIndex :: (Show i, Num i) => i -> String
showIndex i = show (i+0)

verboseList :: POpts -> TT a -> [Tree PE]
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
 where go (Node (PE ValP "") []) = Node (PE ValP s) []
       go (Node p xs) = Node p (map go xs)

-- | Val prism
--
-- >>> _ValE # 123
-- Val 123
--
-- >>> Val 123 ^? _ValE
-- Just 123
--
-- >>> Fail "abc" ^? _ValE
-- Nothing
--
-- >>> Val 1 & _ValE .~ True
-- Val True
--
-- >>> Val False & _ValE %~ not
-- Val True
--
-- >>> Fail "asdF" & _ValE .~ True
-- Fail "asdF"
--
_ValE :: Prism (Val a) (Val b) a b
_ValE = prism Val
         $ \case
              Val a -> Right a
              Fail e -> Left (Fail e)

-- | prism for Val True
--
-- >>> Val True ^? _True
-- Just ()
--
-- >>> Val False ^? _True
-- Nothing
--
_True :: a ~ Bool => Prism' (Val a) ()
_True =
  prism' (const (Val True)) $ \case
                       Val True -> Just ()
                       _ -> Nothing

-- | prism for Val False
--
-- >>> (_True # ()) ^? _True
-- Just ()
--
-- >>> (_False # ()) ^? _False
-- Just ()
--
-- >>> Val False ^? _False
-- Just ()
--
-- >>> Val True ^? _False
-- Nothing
--
_False :: a ~ Bool => Prism' (Val a) ()
_False =
  prism' (const (Val False)) $ \case
                       Val False -> Just ()
                       _ -> Nothing

-- | iso for Val
--
-- >>> Val 123 ^. _ValEither
-- Right 123
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
-- >>> Right False ^. from _ValEither
-- Val False
--
-- >>> [Just (Val 'x')] ^. mapping (mapping _ValEither)
-- [Just (Right 'x')]
--
-- >>> Just (Fail "abcd") ^. mapping _ValEither
-- Just (Left "abcd")
--
_ValEither :: Iso' (Val a) (Either String a)
_ValEither = iso fw bw
  where fw = \case
               Val a -> Right a
               Fail e -> Left e
        bw = either Fail Val

-- | a lens from typed 'Val' to the untyped 'ValP'
--
-- >>> Val True ^. _Val2P
-- ValP
--
-- >>> Val 123 ^. _Val2P
-- ValP
--
-- >>> Fail "abc" ^. _Val2P
-- FailP "abc"
--
_Val2P :: Lens' (Val a) ValP
_Val2P afb bt = bt <$ afb r
  where r = case bt of
              Fail e -> FailP e
              Val {} -> ValP

-- | a lens from typed 'Val' Bool to the untyped 'ValP'
--
-- >>> Val True ^. _Val2BoolP
-- TrueP
--
-- >>> Val False ^. _Val2BoolP
-- FalseP
--
-- >>> Fail "abc" ^. _Val2BoolP
-- FailP "abc"
--
_Val2BoolP :: a ~ Bool => Lens' (Val a) ValP
_Val2BoolP afb bt = bt <$ afb r
  where r = case bt of
              Fail e -> FailP e
              Val True -> TrueP
              Val False -> FalseP



