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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- | promoted character functions
module Predicate.Data.Char (
 -- ** constructor
    C

 -- ** character predicates
  , IsLower
  , IsUpper
  , IsDigit
  , IsSpace
  , IsPunctuation
  , IsControl
  , IsHexDigit
  , IsOctDigit
  , IsSeparator
  , IsLatin1

 -- ** string predicates
  , IsLowerAll
  , IsUpperAll
  , IsDigitAll
  , IsSpaceAll
  , IsPunctuationAll
  , IsControlAll
  , IsHexDigitAll
  , IsOctDigitAll
  , IsSeparatorAll
  , IsLatin1All

 -- ** change case
  , ToTitle
  , ToUpper
  , ToLower
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Control.Lens
import qualified Data.Text.Lens as DTL
import GHC.TypeLits (Symbol, KnownSymbol)
import qualified GHC.TypeLits as GL
import Data.Proxy (Proxy(Proxy))
import Data.Char
import qualified Data.Type.Equality as DE

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> import Predicate

-- | extracts the first character from a non empty 'GHC.TypeLits.Symbol'
--
-- >>> pz @(C "aBc") ()
-- Val 'a'
--
data C (s :: Symbol) deriving Show
instance ( KnownSymbol s
         , FailUnlessT (GL.CmpSymbol s "" DE.== 'GT)
              ('GL.Text "C symbol cannot be empty")
         ) => P (C s) a where
  type PP (C s) a = Char
  eval _ opts _ =
     case symb @s of
       [] -> errorInProgram "C: found empty Symbol/string"
       c:_ -> pure $ mkNode opts (Val c) ("C " <> showL opts c) []


-- | a predicate for determining if a character belongs to the given character set
--
-- >>> pz @(Map '(IsControl, IsLatin1, IsHexDigit, IsOctDigit, IsDigit, IsPunctuation, IsSeparator, IsSpace)) "abc134"
-- Val [(False,True,True,False,False,False,False,False),(False,True,True,False,False,False,False,False),(False,True,True,False,False,False,False,False),(False,True,True,True,True,False,False,False),(False,True,True,True,True,False,False,False),(False,True,True,True,True,False,False,False)]
--
data IsCharSet (cs :: CharSet) deriving Show

instance ( x ~ Char
         , GetCharSet cs
         ) => P (IsCharSet cs) x where
  type PP (IsCharSet cs) x = Bool
  eval _ opts c =
    let msg0 = "Is" ++ drop 1 (show cs)
        (cs,f) = getCharSet @cs
        b = f c
    in pure $ mkNodeB opts b (msg0 <> showVerbose opts " | " ([c] :: String)) []

-- | predicate similar to 'Data.Char.isLower'
--
-- >>> pz @IsLower 'X'
-- Val False
--
-- >>> pz @IsLower '1'
-- Val False
--
-- >>> pz @IsLower 'a'
-- Val True
--
data IsLower deriving Show
type IsLowerT = IsCharSet 'CLower

instance P IsLowerT x => P IsLower x where
  type PP IsLower x = PP IsLowerT x
  eval _ = evalBool (Proxy @IsLowerT)

-- | predicate similar to 'Data.Char.isUpper'
--
data IsUpper deriving Show
type IsUpperT = IsCharSet 'CUpper

instance P IsUpperT x => P IsUpper x where
  type PP IsUpper x = PP IsUpperT x
  eval _ = evalBool (Proxy @IsUpperT)

-- | predicate similar to 'Data.Char.isDigit'
--
-- >>> pz @IsDigit 'g'
-- Val False
--
-- >>> pz @IsDigit '9'
-- Val True
--
data IsDigit deriving Show
type IsDigitT = IsCharSet 'CNumber
instance P IsDigitT x => P IsDigit x where
  type PP IsDigit x = Bool
  eval _ = evalBool (Proxy @IsDigitT)

-- | predicate similar to 'Data.Char.isSpace'
--
-- >>> pz @IsSpace '\t'
-- Val True
--
-- >>> pz @IsSpace ' '
-- Val True
--
-- >>> pz @IsSpace 'x'
-- Val False
--
data IsSpace deriving Show
type IsSpaceT = IsCharSet 'CSpace
instance P IsSpaceT x => P IsSpace x where
  type PP IsSpace x = Bool
  eval _ = evalBool (Proxy @IsSpaceT)

-- | predicate similar to 'Data.Char.isPunctuation'
--
data IsPunctuation deriving Show
type IsPunctuationT = IsCharSet 'CPunctuation
instance P IsPunctuationT x => P IsPunctuation x where
  type PP IsPunctuation x = Bool
  eval _ = evalBool (Proxy @IsPunctuationT)

-- | predicate similar to 'Data.Char.isControl'
--
data IsControl deriving Show
type IsControlT = IsCharSet 'CControl
instance P IsControlT x => P IsControl x where
  type PP IsControl x = Bool
  eval _ = evalBool (Proxy @IsControlT)

-- | predicate similar to 'Data.Char.isHexDigit'
--
-- >>> pz @IsHexDigit 'A'
-- Val True
--
-- >>> pz @IsHexDigit 'g'
-- Val False
--
data IsHexDigit deriving Show
type IsHexDigitT = IsCharSet 'CHexDigit
instance P IsHexDigitT x => P IsHexDigit x where
  type PP IsHexDigit x = Bool
  eval _ = evalBool (Proxy @IsHexDigitT)

-- | predicate similar to 'Data.Char.isOctDigit'
--
data IsOctDigit deriving Show
type IsOctDigitT = IsCharSet 'COctDigit
instance P IsOctDigitT x => P IsOctDigit x where
  type PP IsOctDigit x = Bool
  eval _ = evalBool (Proxy @IsOctDigitT)

-- | predicate similar to 'Data.Char.isSeparator'
--
data IsSeparator deriving Show
type IsSeparatorT = IsCharSet 'CSeparator
instance P IsSeparatorT x => P IsSeparator x where
  type PP IsSeparator x = Bool
  eval _ = evalBool (Proxy @IsSeparatorT)

-- | predicate similar to 'Data.Char.isLatin1'
--
data IsLatin1 deriving Show
type IsLatin1T = IsCharSet 'CLatin1
instance P IsLatin1T x => P IsLatin1 x where
  type PP IsLatin1 x = Bool
  eval _ = evalBool (Proxy @IsLatin1T)


-- | a predicate for determining if a string 'Data.Text.IsText' belongs to the given character set
--
-- >>> pl @('Just Uncons >> IsUpper &* IsLowerAll) "AbcdE"
-- False ((>>) False | {True (&*) False | (IsLowerAll | "bcdE")})
-- Val False
--
-- >>> pl @('Just Uncons >> IsUpper &* IsLowerAll) "Abcde"
-- True ((>>) True | {True (&*) True})
-- Val True
--
-- >>> pl @('Just Uncons >> IsUpper &* IsLowerAll) "xbcde"
-- False ((>>) False | {False (&*) True | (IsUpper | "x")})
-- Val False
--
-- >>> pl @('Just Uncons >> IsUpper &* IsLowerAll) "X"
-- True ((>>) True | {True (&*) True})
-- Val True
--
-- >>> pz @('(IsControlAll, IsLatin1All , IsHexDigitAll , IsOctDigitAll , IsDigitAll , IsPunctuationAll , IsSeparatorAll , IsSpaceAll)) "abc134"
-- Val (False,True,True,False,False,False,False,False)
--
-- >>> pl @(SplitAts [1,2,10] Id >> Para '[IsLowerAll, IsDigitAll, IsUpperAll]) "abdefghi"
-- Present [True,False,False] ((>>) [True,False,False] | {Para(0) [True,False,False] | ["a","bd","efghi"]})
-- Val [True,False,False]
--
-- >>> pl @(SplitAts [1,2,10] Id >> BoolsQuick "" '[IsLowerAll, IsDigitAll, IsUpperAll]) "a98efghi"
-- Error Bool(2) [] (IsUpperAll | "efghi") (["a","98","efghi"])
-- Fail "Bool(2) [] (IsUpperAll | \"efghi\")"
--
-- >>> pl @(SplitAts [1,2,10] Id >> BoolsQuick "" '[IsLowerAll, IsDigitAll, IsUpperAll || IsLowerAll]) "a98efghi"
-- True ((>>) True | {Bools})
-- Val True
--
-- >>> pl @(SplitAts [1,2,10] Id >> BoolsQuick "" '[IsLowerAll, IsDigitAll, IsUpperAll || IsLowerAll]) "a98efgHi"
-- Error Bool(2) [] (False || False | (IsUpperAll | "efgHi") || (IsLowerAll | "efgHi")) (["a","98","efgHi"])
-- Fail "Bool(2) [] (False || False | (IsUpperAll | \"efgHi\") || (IsLowerAll | \"efgHi\"))"
--
data IsCharSetAll (cs :: CharSet) deriving Show

instance ( GetCharSet cs
         , Show a
         , DTL.IsText a
         ) => P (IsCharSetAll cs) a where
  type PP (IsCharSetAll cs) a = Bool
  eval _ opts as =
    let b = allOf DTL.text f as
        msg0 = "Is" ++ drop 1 (show cs) ++ "All"
        (cs,f) = getCharSet @cs
    in pure $ mkNodeB opts b (msg0 <> showVerbose opts " | " as) []

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
             deriving stock (Bounded, Enum, Show, Read, Ord, Eq)

class GetCharSet (cs :: CharSet) where
  getCharSet :: (CharSet, Char -> Bool)
instance GetCharSet 'CLower where
  getCharSet = (CLower, isLower)
instance GetCharSet 'CUpper where
  getCharSet = (CUpper, isUpper)
instance GetCharSet 'CNumber where
  getCharSet = (CNumber, isNumber)
instance GetCharSet 'CSpace where
  getCharSet = (CSpace, isSpace)
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

-- | predicate for determining if a string is all lowercase
--
-- >>> pz @IsLowerAll "abc"
-- Val True
--
-- >>> pz @IsLowerAll "abcX"
-- Val False
--
-- >>> pz @IsLowerAll (T.pack "abcX")
-- Val False
--
-- >>> pz @IsLowerAll "abcdef213"
-- Val False
--
-- >>> pz @IsLowerAll ""
-- Val True
--
data IsLowerAll deriving Show
type IsLowerAllT = IsCharSetAll 'CLower

instance P IsLowerAllT x => P IsLowerAll x where
  type PP IsLowerAll x = PP IsLowerAllT x
  eval _ = evalBool (Proxy @IsLowerAllT)

-- | predicate for determining if a string is all uppercase
data IsUpperAll deriving Show
type IsUpperAllT = IsCharSetAll 'CUpper

instance P IsUpperAllT x => P IsUpperAll x where
  type PP IsUpperAll x = PP IsUpperAllT x
  eval _ = evalBool (Proxy @IsUpperAllT)

-- | predicate for determining if the string is all digits
--
-- >>> pz @IsDigitAll "213G"
-- Val False
--
-- >>> pz @IsDigitAll "929"
-- Val True
--
data IsDigitAll deriving Show
type IsDigitAllT = IsCharSetAll 'CNumber
instance P IsDigitAllT x => P IsDigitAll x where
  type PP IsDigitAll x = Bool
  eval _ = evalBool (Proxy @IsDigitAllT)

-- | predicate for determining if the string is all spaces
--
-- >>> pz @IsSpaceAll "213G"
-- Val False
--
-- >>> pz @IsSpaceAll "    "
-- Val True
--
-- >>> pz @IsSpaceAll ""
-- Val True
--
data IsSpaceAll deriving Show
type IsSpaceAllT = IsCharSetAll 'CSpace
instance P IsSpaceAllT x => P IsSpaceAll x where
  type PP IsSpaceAll x = Bool
  eval _ = evalBool (Proxy @IsSpaceAllT)

-- | predicate for determining if a string has all punctuation
data IsPunctuationAll deriving Show
type IsPunctuationAllT = IsCharSetAll 'CPunctuation
instance P IsPunctuationAllT x => P IsPunctuationAll x where
  type PP IsPunctuationAll x = Bool
  eval _ = evalBool (Proxy @IsPunctuationAllT)

-- | predicate for determining if a string has all control chars
data IsControlAll deriving Show
type IsControlAllT = IsCharSetAll 'CControl
instance P IsControlAllT x => P IsControlAll x where
  type PP IsControlAll x = Bool
  eval _ = evalBool (Proxy @IsControlAllT)

-- | predicate for determining if the string is all hex digits
--
-- >>> pz @IsHexDigitAll "01efA"
-- Val True
--
-- >>> pz @IsHexDigitAll "01egfA"
-- Val False
--
data IsHexDigitAll deriving Show
type IsHexDigitAllT = IsCharSetAll 'CHexDigit
instance P IsHexDigitAllT x => P IsHexDigitAll x where
  type PP IsHexDigitAll x = Bool
  eval _ = evalBool (Proxy @IsHexDigitAllT)

-- | predicate for determining if the string is all octal digits
data IsOctDigitAll deriving Show
type IsOctDigitAllT = IsCharSetAll 'COctDigit
instance P IsOctDigitAllT x => P IsOctDigitAll x where
  type PP IsOctDigitAll x = Bool
  eval _ = evalBool (Proxy @IsOctDigitAllT)

-- | predicate for determining if the string has all separators
data IsSeparatorAll deriving Show
type IsSeparatorAllT = IsCharSetAll 'CSeparator
instance P IsSeparatorAllT x => P IsSeparatorAll x where
  type PP IsSeparatorAll x = Bool
  eval _ = evalBool (Proxy @IsSeparatorAllT)

-- | predicate for determining if the string is all latin chars
data IsLatin1All deriving Show
type IsLatin1AllT = IsCharSetAll 'CLatin1
instance P IsLatin1AllT x => P IsLatin1All x where
  type PP IsLatin1All x = Bool
  eval _ = evalBool (Proxy @IsLatin1AllT)


-- | converts a string 'Data.Text.Lens.IsText' value to lower case
--
-- >>> pz @ToLower "HeLlO wOrld!"
-- Val "hello world!"
--
data ToLower deriving Show

instance ( Show a
         , DTL.IsText a
         ) => P ToLower a where
  type PP ToLower a = a
  eval _ opts as =
    let msg0 = "ToLower"
        xs = as & DTL.text %~ toLower
    in pure $ mkNode opts (Val xs) (show3 opts msg0 xs as) []

-- | converts a string 'Data.Text.Lens.IsText' value to upper case
--
-- >>> pz @ToUpper "HeLlO wOrld!"
-- Val "HELLO WORLD!"
--
data ToUpper deriving Show

instance ( Show a
         , DTL.IsText a
         ) => P ToUpper a where
  type PP ToUpper a = a
  eval _ opts as =
    let msg0 = "ToUpper"
        xs = as & DTL.text %~ toUpper
    in pure $ mkNode opts (Val xs) (show3 opts msg0 xs as) []


-- | converts a string 'Data.Text.Lens.IsText' value to title case
--
-- >>> pz @ToTitle "HeLlO wOrld!"
-- Val "Hello world!"
--
-- >>> data Color = Red | White | Blue | Green | Black deriving (Show,Eq,Enum,Bounded,Read)
-- >>> pz @(ToTitle >> ReadP Color Id) "red"
-- Val Red
--
data ToTitle deriving Show

instance ( Show a
         , DTL.IsText a
         ) => P ToTitle a where
  type PP ToTitle a = a
  eval _ opts as =
    let msg0 = "ToTitle"
        xs = toTitleAll (as ^. DTL.unpacked) ^. DTL.packed
    in pure $ mkNode opts (Val xs) (show3 opts msg0 xs as) []


toTitleAll :: String -> String
toTitleAll (x:xs) = toUpper x : map toLower xs
toTitleAll [] = []

