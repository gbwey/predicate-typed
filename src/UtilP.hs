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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
-- |
-- Module      : UtilP
-- Description : Utility methods for Predicate / methods for displaying the evaluation tree ...
-- Copyright   : (c) Grant Weyburne, 2019
-- License     : BSD-3
-- Maintainer  : gbwey9@gmail.com
--
module UtilP where
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
import Data.Tree.Pretty
import Data.Proxy
import Data.List.NonEmpty (NonEmpty(..))
import Data.Char
import Data.Data
import System.Console.Pretty
import qualified Text.PrettyPrint as PP
import qualified Data.Type.Equality as DE
import GHC.Exts (Constraint)
import qualified Text.Regex.PCRE.Heavy as RH
import qualified Text.Regex.PCRE.Light as RL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Discrimination as D
import qualified Data.Text as T
import Data.ByteString (ByteString)
import GHC.Word (Word8)
import Data.Sequence (Seq)
import Control.Applicative (ZipList)
import Data.Kind (Type)
import Data.Either
import Data.These
import qualified Control.Exception as E
import Control.DeepSeq
import System.IO.Unsafe (unsafePerformIO)
import Data.Bool
import Data.Foldable

-- | describes the evaluation tree for predicates
data TT a = TT { _tBool :: BoolT a  -- ^ the value at this root node
               , _tStrings :: [String]  -- ^ detailed information eg input and output and text
               , _tForest :: Forest PE } -- ^ the child nodes
                deriving Show

-- | contains the typed result from evaluating the expression tree to this point
data BoolT a where
  FailT :: String -> BoolT a  -- ^ failure with string
  FalseT :: BoolT Bool        -- ^ false predicate
  TrueT :: BoolT Bool         -- ^ true predicate
  PresentT :: a -> BoolT a    -- ^ non predicate value

deriving instance Show a => Show (BoolT a)
deriving instance Eq a => Eq (BoolT a)

tBool :: Lens (TT a) (TT b) (BoolT a) (BoolT b)
tBool afb s = (\b -> s { _tBool = b }) <$> afb (_tBool s)

tStrings :: Lens' (TT a) [String]
tStrings afb s = (\b -> s { _tStrings = b }) <$> afb (_tStrings s)

tForest :: Lens' (TT a) (Forest PE)
tForest afb s = (\b -> s { _tForest = b }) <$> afb (_tForest s)

pStrings :: Lens' PE [String]
pStrings afb s = (\b -> s { _pStrings = b }) <$> afb (_pStrings s)

-- | a lens from typed BoolT to the untyped BoolP
boolT2P :: Lens' (BoolT a) BoolP
boolT2P afb = \case
  FailT e -> FailT e <$ afb (FailP e)
  TrueT -> TrueT <$ afb TrueP
  FalseT -> FalseT <$ afb FalseP
  PresentT a -> PresentT a <$ afb PresentP

-- | contains the untyped result from evaluating the expression tree to this point
data BoolP =
    FailP String
  | FalseP
  | TrueP
  | PresentP
  deriving (Show, Eq)

-- need a semigroup constraint else we have to throw away one of the PresentT a ie First or Last
instance Semigroup a => Semigroup (BoolT a) where
  FailT e <> FailT e1 = FailT (e <> e1)
  o@FailT {} <> _ = o
  _ <> o@FailT {} = o
  o@TrueT <> TrueT = o
  o@FalseT <> _ = o
  _ <> o@FalseT = o
  -- cant pattern match on PresentT True on lhs (hence PresentT a) but can use 'a' as a Bool on rhs!
  PresentT a <> TrueT = review _boolT a
  TrueT <> PresentT a = review _boolT a
  PresentT a <> PresentT a1 = PresentT (a <> a1)

instance Monoid a => Monoid (BoolT a) where
  mempty = PresentT mempty

data PE = PE { _pBool :: BoolP -- ^ holds the result of running the predicate
             , _pStrings :: [String] -- ^ optional strings to include in the results
             } deriving Show

pBool :: Lens' PE BoolP
pBool afb (PE x y) = flip PE y <$> afb x

-- | creates a Node for the evaluation tree
mkNode :: POpts -> BoolT a -> [String] -> [Holder] -> TT a
mkNode opts bt ss hs
  | oLite opts = TT bt [] []
  | otherwise = TT bt ss (map fromTTH hs)

-- | creates a Boolean node for a predicate type
mkNodeB :: POpts -> Bool -> [String] -> [Holder] -> TT Bool
mkNodeB opts b = mkNode opts (bool FalseT TrueT b)

-- | partition a tree into failures and non failures
partitionTTs :: [TT a] -> ([TT x], [TT a])
partitionTTs = partitionEithers . map getTTLR

getTTLR :: TT a -> Either (TT x) (TT a)
getTTLR t =
  case _tBool t of
    FailT e -> Left $ t & tBool .~ FailT e
    _ -> Right t

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

shortTT :: BoolT Bool -> Either String String
shortTT z = case z of
    FailT e -> Left $ "FailT " <> e
    TrueT -> Right $ show z
    FalseT -> Left $ show z
    PresentT True -> Right $ show z
    PresentT False -> Left $ show z

-- | converts a typed tree to an untyped on for display
fromTT :: TT a -> Tree PE
fromTT (TT bt ss tt) = Node (PE (bt ^. boolT2P) ss) tt

-- | a monomorphic container of trees
data Holder = forall w . Holder { unHolder :: TT w }

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
newtype PColor = PColor { unPColor :: BoolP -> String -> String }

-- | customizable options
data POpts = POpts { oShowA :: Maybe Int -- ^ length of data to display for 'showA'
                   , oDebug :: !Int  -- ^ debug level
                   , oDisp :: Disp -- ^ how to display the tree orientation and unicode etc
                   , oHide :: !Int -- ^ hides one layer of a tree
                   , oColor :: !(String, PColor) -- ^ color palette used
                   , oLite :: !Bool
                   }

-- | display format for the tree
data Disp = NormalDisp -- ^ draw horizontal tree
          | Vertical !Int -- ^ draw vertical tree
          | Unicode  -- ^ use unicode
          | PPTree  -- ^ pretty printer tree
          deriving (Show, Eq)

instance Show POpts where
  show POpts {..} =
    "POpts: showA=" <> show oShowA
    <> " debug=" <> show oDebug
    <> " disp=" <> show oDisp
    <> " hide=" <> show oHide
    <> " color=" <> show (fst oColor)
    <> " lite=" <> show oLite

defOpts :: POpts
defOpts = POpts
    { oShowA = Just 110
    , oDebug = 0
    , oDisp = NormalDisp
    , oHide = 0
    , oColor = color1
    , oLite = False
    }

-- | skip colors and just return the summary
ol :: POpts
ol = o0 { oColor = color0, oLite = True }

-- | skip the detail and just return the summary but keep the colors
olc :: POpts
olc = ol { oColor = color1 }

o0 :: POpts
o0 = defOpts { oColor = color0 }

-- | skip colors
o02 :: POpts
o02 = o2 { oColor = color0 }

o03 :: POpts
o03 = o3 { oColor = color0 }


o1 :: POpts
o1 = defOpts { oDebug = 1, oShowA = Just 120 }

-- | colors and details
o2 :: POpts
o2 = defOpts { oDebug = 2, oShowA = Just 200 }

o3 :: POpts
o3 = defOpts { oDebug = 3, oShowA = Just 400 }


-- | helper method to set the width of data to be shown in the tree
seta :: Int -> POpts -> POpts
seta w o = o { oShowA = Just w }

-- | helper method to display the tree vertically
setv :: Int -> POpts -> POpts
setv w o = o { oDisp = Vertical w }

-- | helper method to set the debug level
setd :: Int -> POpts -> POpts
setd v o = o { oDebug = v }

setu :: POpts -> POpts
setu o = o { oDisp = Unicode }

setc :: (String, PColor) -> POpts -> POpts
setc pc o = o { oColor = pc }

setc0, setc1, setc2, setc3, setc4 :: POpts -> POpts
setc0 o = o { oColor = color0 }
setc1 o = o { oColor = color1 }
setc2 o = o { oColor = color2 }
setc3 o = o { oColor = color3 }
setc4 o = o { oColor = color4 }

-- | italics dont work but underline does
-- | color palettes
color0, color1, color2, color3, color4 :: (String, PColor)
color0 = ("color0", PColor $ flip const)

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

defh, defv, defu :: POpts
defh = o1
defv = defv' defaultGap
defu = setu o1

defv' :: Width -> POpts
defv' w = setv w o1

-- | fix PresentT Bool to TrueT or FalseT
fixBoolT :: TT Bool -> TT Bool
fixBoolT t =
  case t ^? tBool . _PresentT of
    Nothing -> t
    Just b -> t & tBool .~ _boolT # b

showLit0 :: POpts -> String -> String -> String
showLit0 o s a = showLit' o 0 s a

showLit3 :: POpts -> String -> String -> String
showLit3 o s a = showLit' o 3 s a

showLit :: POpts -> String -> String -> String
showLit o s a = showLit' o 1 s a

showLit' :: POpts -> Int -> String -> String -> String
showLit' o i s a =
  if oDebug o >= i then
    let f n = let ss = take n a
              in ss <> (if length ss==n then " ..." else "")
    in maybe "" (\n -> s <> f n) (oShowA o)
  else ""

show0 :: Show a => POpts -> String -> a -> String
show0 o s a = showA' o 0 s a

show3 :: Show a => POpts -> String -> a -> String
show3 o s a = showA' o 3 s a

showA :: Show a => POpts -> String -> a -> String
showA o s a = showA' o 1 s a

showA' :: Show a => POpts -> Int -> String -> a -> String
showA' o i s a = showLit' o i s (show a)

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
compileRegex :: forall s rs a . (KnownSymbol s, GetROpts rs)
  => POpts -> String -> Either (TT a) RH.Regex
compileRegex opts nm =
    let s = symb @s
        rs = getROpts @rs
        mm = nm <> " " <> show rs
    in flip left (RH.compileM (B8.pack s) rs)
          $ \e -> mkNode opts (FailT "Regex failed to compile") [mm <> " compile failed with regex msg[" <> e <> "]"] []

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
formatList opts = unwords . map (\((i, a), _) -> "(i=" <> show i <> showA' opts 0 ", a=" a <> ")")

-- | extract all root values from a list of trees
valsFromTTs :: [TT a] -> [a]
valsFromTTs = concatMap toList

instance Foldable TT where
  foldMap am = foldMap am . _tBool

instance Foldable BoolT where
  foldMap am = either (const mempty) am . getValLR

isTrue :: BoolT Bool -> Bool
isTrue = and
--isTrue = or

-- cant use: is / isn't / has cos only FailT will be False: use Fold
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
groupErrors = intercalate " | " . map (\xs -> head xs <> (if length xs > 1 then "(" <> show (length xs) <> ")" else "")) . D.group

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

imply :: Bool -> Bool -> Bool
imply p q = not p || q

-- msg is only used for an exception: up to the calling programs to deal with ading msg to good and bad
-- | applies a boolean binary operation against the values from two boolean trees
evalBinStrict :: POpts
                 -> String
                 -> (Bool -> Bool -> Bool)
                 -> TT Bool
                 -> TT Bool
                 -> TT Bool
evalBinStrict opts s fn ll rr =
  case getValueLR opts (s <> " p") ll [Holder rr] of
    Left e -> e
    Right a ->
      case getValueLR opts (s <> " q") rr [hh ll] of
        Left e -> e
        Right b ->
          let z = fn a b
          in mkNodeB opts z [show a <> " " <> s <> " " <> show b] [hh ll, hh rr]

-- | type level Between
type family BetweenT (a :: Nat) (b :: Nat) (v :: Nat) :: Constraint where
  BetweenT m n v =
     FailIfT (NotT (AndT (m GL.<=? v) (v GL.<=? n)))
            ('GL.Text "BetweenT failure"
             ':$$: 'GL.ShowType v
             ':$$: 'GL.Text " is outside of "
             ':$$: 'GL.ShowType m
             ':<>: 'GL.Text " and "
             ':<>: 'GL.ShowType n)

-- | makes zero invalid at the type level
type NotZeroT v = FailIfT (v DE.== 0) ('GL.Text "found zero value")

-- | typelevel Null on Symbol
type family NullT (x :: Symbol) :: Bool where
  NullT ("" :: Symbol) = 'True
  NullT _ = 'False

-- | helper method to fail with an error if the True
type family FailIfT (b :: Bool) (msg :: GL.ErrorMessage) :: Constraint where
  FailIfT 'False _ = ()
  FailIfT 'True e = GL.TypeError e

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
nat :: forall n a . (KnownNat n, Num a) => a
nat = fromIntegral (GL.natVal (Proxy @n))

-- | gets the Symbol from the typelevel
symb :: forall s . KnownSymbol s => String
symb = GL.symbolVal (Proxy @s)

-- | get a list of Nats from the typelevel
class GetNats as where
  getNats :: [Int]
instance GetNats '[] where
  getNats = []
instance (KnownNat n, GetNats ns) => GetNats (n ': ns) where
  getNats = nat @n : getNats @ns

-- | get a list of Symbols from the typelevel
class GetSymbs ns where
  getSymbs :: [String]
instance GetSymbs '[] where
  getSymbs = []
instance (KnownSymbol s, GetSymbs ss) => GetSymbs (s ': ss) where
  getSymbs = symb @s : getSymbs @ss

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

data N = S N | Z

-- a shim for TupleListImpl used mainly by Printfn
-- | inductive numbers
type family ToN (n :: Nat) :: N where
  ToN 0 = 'Z
  ToN n = 'S (ToN (n GL.- 1))

-- | converts an inductive number to Nat
type family FromN (n :: N) :: Nat where
  FromN 'Z = 0
  FromN ('S n) = 1 GL.+ FromN n

-- | extract N from the type level to Int
class GetNatN (n :: N) where
  getNatN :: Int
instance GetNatN 'Z where
  getNatN = 0
instance GetNatN n => GetNatN ('S n) where
  getNatN = 1 + getNatN @n

getN :: Typeable t => Proxy (t :: N) -> Int
getN p = length (show (typeRep p)) `div` 5

data OrderingP = Cgt | Cge | Ceq | Cle | Clt | Cne deriving (Show, Eq, Enum, Bounded)

class GetOrd (k :: OrderingP) where
  getOrd :: Ord a => (String, a -> a -> Bool)

instance GetOrd 'Cgt where getOrd = (">", (>))
instance GetOrd 'Cge where getOrd = (">=",(>=))
instance GetOrd 'Ceq where getOrd = ("==",(==))
instance GetOrd 'Cle where getOrd = ("<=",(<=))
instance GetOrd 'Clt where getOrd = ("<", (<))
instance GetOrd 'Cne where getOrd = ("/=",(/=))

-- only hides BoolP part! not sure of the point
toNodeString :: POpts -> PE -> String
toNodeString opts bpe
  | oLite opts = error $ "shouldnt be calling this if we are going lite: toNodeString oLite " ++ show bpe
  | otherwise = showBoolP opts (_pBool bpe) <> " " <> displayMessages (_pStrings bpe)

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
prtTree o = prtImpl o . fmap (toNodeString o)

prtImpl :: POpts -> Tree String -> IO ()
prtImpl = (putStr .) . showImpl

fixLite :: forall a . Show a => POpts -> a -> Tree PE -> String
fixLite opts a t
  | oLite opts = fixPresentP opts (t ^. root . pBool) a <> "\n"
  | otherwise = prtTreePure opts t

fixPresentP :: Show a => POpts -> BoolP -> a -> String
fixPresentP opts bp a =
  case bp of
    PresentP -> colorMe opts PresentP "Present" <> " " <> show a
    _ -> showBoolP opts bp

prtTreePure :: POpts -> Tree PE -> String
prtTreePure opts t
  | oLite opts = showBoolP opts (t ^. root . pBool) <> "\n"
  | otherwise = showImpl opts $ fmap (toNodeString opts) t

showImpl :: POpts -> Tree String -> String
showImpl o =
  case oDisp o of
    Unicode -> TV.showTree
    NormalDisp -> drawTree -- to drop the last newline else we have to make sure that everywhere else has that newline: eg fixLite
    Vertical w -> drawVerticalTreeWith w
    PPTree -> (<>"\n") . PP.render . ppTree PP.text -- no newlines!

lite :: POpts -> POpts
lite o = o { oLite = True }

unicode :: POpts -> POpts
unicode o = o { oDisp = Unicode }

horizontal :: POpts -> POpts
horizontal o = o { oDisp = NormalDisp }

vertical :: POpts -> POpts
vertical = vertical' defaultGap

vertical' :: Width -> POpts -> POpts
vertical' w o = o { oDisp = Vertical w }


-- | display in document in tree format
ppTree :: (a -> PP.Doc) -> Tree a -> PP.Doc
ppTree pp = ppT
  where
    ppT (Node x []) = pp x
    ppT (Node x xs) = PP.parens $ PP.hang (pp x) 2 $
        PP.sep $ map ppT xs


prettyRational :: Rational -> String
prettyRational (numerator &&& denominator -> (n,d)) =
  if | n == 0 -> "0"
     | d == 1 -> show n
     | otherwise -> show n <> " / " <> show d

fixit :: ((Int, x), TT a) -> TT a
fixit ((i, _), t) = prefixMsg ("i=" <> show i <> ":") t

prefixMsg :: String -> TT a -> TT a
prefixMsg msg t =
   t & tStrings .ix 0 %~ (msg <>)

showNat :: forall n . KnownNat n => String
showNat = show (nat @n :: Integer)

showT :: forall (t :: Type) . Typeable t => String
showT = show (typeRep (Proxy @t))

showTProxy :: forall p . Typeable (Proxy p) => String
showTProxy = drop 8 $ show (typeOf (Proxy @p))

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


type NEmptyT k = ('[] ':| '[] :: NonEmpty [k])

isPrime :: Integer -> Bool
isPrime n = n==2 || n>2 && all ((> 0).rem n) (2:[3,5 .. floor . sqrt @Double . fromIntegral $ n+1])

type family TupleListT (n :: N) a where
  TupleListT 'Z a = ()
  TupleListT ('S n) a = (a, TupleListT n a)

class TupleListD (n :: N) a where
  tupleListD :: Bool -> [a] -> Either String (TupleListT n a)

instance TupleListD 'Z a where
  tupleListD isStrict = \case
     z@(_:_) | isStrict ->
       let len = length z
       in Left $ "is strict and has " <> show len <> " extra element" <> (if len == 1 then "" else "s")
     _ -> Right ()

instance (TupleListD n a) => TupleListD ('S n) a where
  tupleListD isStrict = \case
    [] -> Left "no data left" -- nothing i can do here even if not strict
    a:as -> (a,) <$> tupleListD @n @a isStrict as

-- up to 12
class ReverseTupleC x where
  type ReverseTupleP x
  reverseTupleC :: x -> ReverseTupleP x
instance (GL.TypeError ('GL.Text "ReverseTupleC: inductive tuple cannot be empty")) => ReverseTupleC () where
  type ReverseTupleP () = ()
  reverseTupleC () = ()
instance ReverseTupleC (a,()) where
  type ReverseTupleP (a,()) = (a,())
  reverseTupleC (a,()) = (a,())
instance ReverseTupleC (a,(b,())) where
  type ReverseTupleP (a,(b,())) = (b,(a,()))
  reverseTupleC (a,(b,())) = (b,(a,()))
instance ReverseTupleC (a,(b,(c,()))) where
  type ReverseTupleP (a,(b,(c,()))) = (c,(b,(a,())))
  reverseTupleC (a,(b,(c,()))) = (c,(b,(a,())))
instance ReverseTupleC (a,(b,(c,(d,())))) where
  type ReverseTupleP (a,(b,(c,(d,())))) = (d,(c,(b,(a,()))))
  reverseTupleC (a,(b,(c,(d,())))) = (d,(c,(b,(a,()))))
instance ReverseTupleC (a,(b,(c,(d,(e,()))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,()))))) = (e,(d,(c,(b,(a,())))))
  reverseTupleC (a,(b,(c,(d,(e,()))))) = (e,(d,(c,(b,(a,())))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,())))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,())))))) = (f,(e,(d,(c,(b,(a,()))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,())))))) = (f,(e,(d,(c,(b,(a,()))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,()))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,()))))))) = (g,(f,(e,(d,(c,(b,(a,())))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,()))))))) = (g,(f,(e,(d,(c,(b,(a,())))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,())))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,(h,())))))))) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,())))))))) = (h,(g,(f,(e,(d,(c,(b,(a,()))))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,()))))))))) = (i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,())))))))))) = (j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,()))))))))))) = (k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,())))))))))))
instance ReverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) where
  type ReverseTupleP (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))
  reverseTupleC (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,())))))))))))) = (l,(k,(j,(i,(h,(g,(f,(e,(d,(c,(b,(a,()))))))))))))

-- a hack to get 'a' from '[a]' which I need for type PP
type family ArrT (as :: Type) :: Type where
  ArrT [a] = a
  ArrT as = GL.TypeError (
      'GL.Text "ArrT: expected [a] but found something else"
      ':$$: 'GL.Text "as = "
      ':<>: 'GL.ShowType as)

type family TupleLenT (t :: Type) :: Nat where
  TupleLenT () = 0
  TupleLenT (_,ts) = 1 GN.+ TupleLenT ts
  TupleLenT  t = GL.TypeError (
      'GL.Text "TupleLenT: expected a valid inductive tuple"
      ':$$: 'GL.Text "t = "
      ':<>: 'GL.ShowType t)

-- partially apply the 2nd arg to an ADT -- $ and & work with functions only
-- doesnt apply more than once because we need to eval it
type family (p :: k -> k1) % (q :: k) :: k1 where
  p % q = p q

infixl 9 %

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

type family ConsT s where
  ConsT [a] = a
  ConsT (ZipList a) = a
  ConsT T.Text = Char
  ConsT ByteString = Word8
  ConsT (Seq a) = a
  ConsT s  = GL.TypeError (
      'GL.Text "ConsT: not a valid ConsT"
      ':$$: 'GL.Text "s = "
      ':<>: 'GL.ShowType s)

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

