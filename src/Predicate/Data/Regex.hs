{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- | promoted regular expression functions
module Predicate.Data.Regex (
    Re
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
 ) where
import Predicate.Core
import Predicate.Misc
import Predicate.Util
import Data.Proxy (Proxy(Proxy))
import qualified Text.Regex.PCRE.Heavy as RH

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> import Safe (readNote)
-- >>> import Predicate.Prelude
-- >>> import Data.Time

-- | runs a regular expression with given regex options and returns a boolean: see 'RH.=~'
--
-- >>> pl @(Re' '[ 'Caseless, 'Dotall ] "ab" Id) "aB"
-- True (Re' ['Caseless, 'Dotall] (ab) | aB)
-- Val True
--
-- >>> pl @(Re' '[ 'Caseless, 'Dotall ] "ab." Id) "aB\n"
-- True (Re' ['Caseless, 'Dotall] (ab.) | aB
-- )
-- Val True
--
-- >>> pl @(Re' '[ 'Caseless ] "ab." Id) "aB\n"
-- False (Re' ['Caseless] (ab.) | aB
-- )
-- Val False
--
data Re' (rs :: [ROpt]) p q deriving Show

instance ( GetROpts rs
         , PP p x ~ String
         , PP q x ~ String
         , P p x
         , P q x
         ) => P (Re' rs p q) x where
  type PP (Re' rs p q) x = Bool
  eval _ opts x = do
    let msg0 = "Re" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs msg1 p of
            Left (e,e1) -> mkNode opts (Fail e) e1 hhs
            Right regex ->
               let b = q RH.=~ regex
               in mkNodeB opts b (msg1 <> litVerbose opts " | " q) hhs


-- | runs a regular expression and returns a boolean: see 'RH.=~'
--
-- >>> pz @(Re "^\\d{2}:\\d{2}:\\d{2}$") "13:05:25"
-- Val True
--
-- >>> pl @(Re "\\d{4}-\\d{3}") "1234-123"
-- True (Re (\d{4}-\d{3}) | 1234-123)
-- Val True
--
-- >>> pl @(Re "\\d{4}-\\d{3}") "1234-1x3"
-- False (Re (\d{4}-\d{3}) | 1234-1x3)
-- Val False
--
-- >>> pl @(Re "(?i)ab") "aB" -- runtime [use 'Caseless instead]
-- True (Re ((?i)ab) | aB)
-- Val True
--
-- >>> pl @(Re "ab") "aB"
-- False (Re (ab) | aB)
-- Val False
--
-- >>> pl @(Re "^\\d{1,3}(?:\\.\\d{1,3}){3}$") "123.1.1.21"
-- True (Re (^\d{1,3}(?:\.\d{1,3}){3}$) | 123.1.1.21)
-- Val True
--
-- >>> pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$") >> ReadP Double Id) "13.345"
-- Present 13.345 ((>>) 13.345 | {ReadP Double 13.345})
-- Val 13.345
--
-- >>> pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$") >> ReadP Double Id) "13"
-- Present 13.0 ((>>) 13.0 | {ReadP Double 13.0})
-- Val 13.0
--
-- >>> pl @(ExitWhen "regex failed" (Not (Re "^\\d+(?:\\.\\d+)?$")) >> ReadP Double Id) "-13.4"
-- Error regex failed (Guard | "-13.4")
-- Fail "regex failed"
--
-- >>> pl @(Re "\\d{4}\\") "ayx"
-- Error Regex failed to compile (Re (\d{4}\) ([],[]):\ at end of pattern)
-- Fail "Regex failed to compile"
--
-- >>> pl @(Re "^\\d+$") "123\nx"
-- False (Re (^\d+$) | 123
-- x)
-- Val False
--
-- >>> pl @(Re "(?m)^\\d+$") "123\nx" -- (?m) anchors match beginning/end of line instead of whole string
-- True (Re ((?m)^\d+$) | 123
-- x)
-- Val True
--
-- >>> pl @(Catch (Re "\\d+(") 'False) "123"
-- False (Catch caught exception[Regex failed to compile])
-- Val False
--
-- >>> pl @(Catch (Re "\\d+") 'False) "123"
-- True (Catch did not fire)
-- Val True
--
data Re p deriving Show
type ReT p = Re' '[] p Id

instance P (ReT p) x => P (Re p) x where
  type PP (Re p) x = PP (ReT p) x
  eval _ = evalBool (Proxy @(ReT p))

-- only way with rescan is to be explicit: no repeats! and useanchors but not (?m)
-- or just use Re' but then we only get a bool ie doesnt capture groups
-- rescan returns Right [] as an failure!
-- [] is failure!
--  anchored means it has to start at the beginning: can have junk on the end which we cant detect but at least we know it starts at beginning


-- | runs a regex matcher returning the original values and optionally any groups: see 'RH.scan'
--
-- >>> pl @(Rescan' '[ 'Anchored ] "([[:xdigit:]]{2})" Id) "wfeb12az"
-- Error Regex no results (Rescan' ['Anchored] (([[:xdigit:]]{2})) | "wfeb12az")
-- Fail "Regex no results"
--
-- >>> pz @(Rescan' '[] Snd "13:05:25") ('a',"^(\\d{2}):(\\d{2}):(\\d{2})$")
-- Val [("13:05:25",["13","05","25"])]
--
data Rescan' (rs :: [ROpt]) p q deriving Show

instance ( GetROpts rs
         , PP p x ~ String
         , PP q x ~ String
         , P p x
         , P q x
         ) => P (Rescan' rs p q) x where
  type PP (Rescan' rs p q) x = [(String, [String])]
  eval _ opts x = do
    let msg0 = "Rescan" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs msg1 p of
             Left (e,e1) -> mkNode opts (Fail e) e1 hhs
             Right regex ->
               case splitAt (oRecursion opts) $ RH.scan regex q of
                 (b, _:_) -> mkNode opts (Fail ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
                 ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                             mkNode opts (Fail "Regex no results") (msg1 <> showVerbose opts " | " q) [hh pp, hh qq]
                 (b, _) -> mkNode opts (Val b) (lit3 opts msg1 b "" q) [hh pp, hh qq]

-- | see 'RH.scan'
--
-- >>> pz @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$") "13:05:25"
-- Val [("13:05:25",["13","05","25"])]
--
-- >>> pz @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$" >> L2 Head >> Map (ReadP Int Id)) "13:05:25"
-- Val [13,5,25]
--
-- >>> pl @(Rescan "(\\d+)\\D?" >> Map (Second (ReadP Int (OneP)))) "123-444-987"
-- Present [("123-",123),("444-",444),("987",987)] ((>>) [("123-",123),("444-",444),("987",987)] | {Map [("123-",123),("444-",444),("987",987)] | [("123-",["123"]),("444-",["444"]),("987",["987"])]})
-- Val [("123-",123),("444-",444),("987",987)]
--
-- >>> pl @(Rescan ".(.)") "aBcd"
-- Present [("aB",["B"]),("cd",["d"])] (Rescan (.(.)) [("aB",["B"]),("cd",["d"])] | aBcd)
-- Val [("aB",["B"]),("cd",["d"])]
--
-- >>> pl @(Rescan "\\d{1,3}(\\.)?") "123.8.99.21"
-- Present [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])] (Rescan (\d{1,3}(\.)?) [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])] | 123.8.99.21)
-- Val [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])]
--
-- >>> pl @(Map' Fst (Rescan "." << ShowP Id) >> Filter (Same "2") Id) 12324
-- Present ["2","2"] ((>>) ["2","2"] | {Fst ["2","2"] | (["2","2"],["1","3","4"])})
-- Val ["2","2"]
--
-- >>> pl @(Rescan "(\\d)+?") "1234"
-- Present [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])] (Rescan ((\d)+?) [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])] | 1234)
-- Val [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])]
--
-- >>> pl @(Rescan "(\\d)+") "1234"
-- Present [("1234",["4"])] (Rescan ((\d)+) [("1234",["4"])] | 1234)
-- Val [("1234",["4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?") "1.2.3.4" -- overcapturing
-- Present [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] (Rescan ((\d{1,3})(\.(\d{1,3}))+?) [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] | 1.2.3.4)
-- Val [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]
--
-- >>> pl @(Rescan "^(\\d)+?$") "1234"
-- Present [("1234",["4"])] (Rescan (^(\d)+?$) [("1234",["4"])] | 1234)
-- Val [("1234",["4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?") "1.2.3.4"
-- Present [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] (Rescan ((\d{1,3})(\.(\d{1,3}))+?) [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] | 1.2.3.4)
-- Val [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(?:\\.(\\d{1,3}))+?") "1.2.3.4" -- bizzare!
-- Present [("1.2",["1","2"]),("3.4",["3","4"])] (Rescan ((\d{1,3})(?:\.(\d{1,3}))+?) [("1.2",["1","2"]),("3.4",["3","4"])] | 1.2.3.4)
-- Val [("1.2",["1","2"]),("3.4",["3","4"])]
--
-- >>> pl @(Rescan "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$") "1.2.3.4"
-- Present [("1.2.3.4",["1","2","3","4"])] (Rescan (^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$) [("1.2.3.4",["1","2","3","4"])] | 1.2.3.4)
-- Val [("1.2.3.4",["1","2","3","4"])]
--
-- >>> pl @(Rescan "([[:xdigit:]]{2})") "wfeb12az"
-- Present [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])] (Rescan (([[:xdigit:]]{2})) [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])] | wfeb12az)
-- Val [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])]
--
data Rescan p deriving Show
type RescanT p = Rescan' '[] p Id

instance P (RescanT p) x => P (Rescan p) x where
  type PP (Rescan p) x = PP (RescanT p) x
  eval _ = eval (Proxy @(RescanT p))


data RescanRanges' (rs :: [ROpt]) p q deriving Show

instance ( GetROpts rs
         , PP p x ~ String
         , PP q x ~ String
         , P p x
         , P q x
         ) => P (RescanRanges' rs p q) x where
  type PP (RescanRanges' rs p q) x = [((Int,Int), [(Int,Int)])]
  eval _ opts x = do
    let msg0 = "RescanRanges" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs msg1 p of
          Left (e,e1) -> mkNode opts (Fail e) e1 hhs
          Right regex ->
            case splitAt (oRecursion opts) $ RH.scanRanges regex q of
              (b, _:_) -> mkNode opts (Fail ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (Fail "Regex no results") (msg1 <> showVerbose opts " | " q) hhs
              (b, _) -> mkNode opts (Val b) (lit3 opts msg1 b "" q) hhs

-- | see 'RH.scanRanges'
--
-- >>> pz @(RescanRanges "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
-- Val [((0,8),[(0,2),(3,5),(6,8)])]
--
data RescanRanges p q deriving Show
type RescanRangesT p q = RescanRanges' '[] p q

instance P (RescanRangesT p q) x => P (RescanRanges p q) x where
  type PP (RescanRanges p q) x = PP (RescanRangesT p q) x
  eval _ = eval (Proxy @(RescanRangesT p q))

-- | splits a string on a regex delimiter: see 'RH.split'
--
-- >>> pl @(Resplit' '[ 'Caseless ] "aBc" Id) "123AbC456abc"
-- Present ["123","456",""] (Resplit' ['Caseless] (aBc) ["123","456",""] | 123AbC456abc)
-- Val ["123","456",""]
--
-- >>> pz @(Resplit' '[] (Singleton Fst) Snd) (':', "12:13:1")
-- Val ["12","13","1"]
--
data Resplit' (rs :: [ROpt]) p q deriving Show

instance ( GetROpts rs
         , PP p x ~ String
         , PP q x ~ String
         , P p x
         , P q x
         ) => P (Resplit' rs p q) x where
  type PP (Resplit' rs p q) x = [String]
  eval _ opts x = do
    let msg0 = "Resplit" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs msg1 p of
          Left (e,e1) -> mkNode opts (Fail e) e1 hhs
          Right regex ->
            case splitAt (oRecursion opts) $ RH.split regex q of
              (b, _:_) -> mkNode opts (Fail ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (Fail "Regex no results") (msg1 <> showVerbose opts " | " q) hhs
              (b, _) -> mkNode opts (Val b) (lit3 opts msg1 b "" q) hhs

-- | splits a string on a regex delimiter: see 'RH.split'
--
-- >>> pz @(Resplit "\\.") "141.201.1.22"
-- Val ["141","201","1","22"]
--
-- >>> pl @(Resplit "\\.") "123.2.3.5.6"
-- Present ["123","2","3","5","6"] (Resplit (\.) ["123","2","3","5","6"] | 123.2.3.5.6)
-- Val ["123","2","3","5","6"]
--
-- >>> pl @(Map' (ReadP Int Id) (Resplit "\\.") >> '(Id, '(Len == 4, All (0 <..> 0xff)))) "141.214.125.1.2.3333"
-- Present ([141,214,125,1,2,3333],(False,False)) ((>>) ([141,214,125,1,2,3333],(False,False)) | {'([141,214,125,1,2,3333],(False,False))})
-- Val ([141,214,125,1,2,3333],(False,False))
--
-- >>> pl @(Map' (ReadP Int Id) (Resplit "\\.") >> Id &&& ((Len == 4) &&& All (0 <..> 0xff))) "141.214.125.1.2.6"
-- Present ([141,214,125,1,2,6],(False,True)) ((>>) ([141,214,125,1,2,6],(False,True)) | {'([141,214,125,1,2,6],(False,True))})
-- Val ([141,214,125,1,2,6],(False,True))
--
-- >>> pl @(Resplit "\\." >> Map (ReadP Int Id) >> Id &&& ((Len == 4) &&& All (0 <..> 0xff))) "141.214.125."
-- Error ReadP Int () (Map(i=3, a="") excnt=1)
-- Fail "ReadP Int ()"
--
data Resplit p deriving Show
type ResplitT p = Resplit' '[] p Id

instance P (ResplitT p) x => P (Resplit p) x where
  type PP (Resplit p) x = PP (ResplitT p) x
  eval _ = eval (Proxy @(ResplitT p))

-- | replaces regex @s@ with a string @s1@ inside the value: see 'RH.sub' and 'RH.gsub'
--
-- >>> pz @(ReplaceAllString 'ROverWrite "\\." ":" Id) "141.201.1.22"
-- Val "141:201:1:22"
--
data ReplaceImpl (alle :: Bool) (rs :: [ROpt]) p q r deriving Show

instance ( GetBool b
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
    let msg0 = "Replace" <> (if alle then "All" else "One") <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
        alle = getBool @b
    lr <- runPQ NoInline msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs msg1 p of
          Left (e,e1) -> pure $ mkNode opts (Fail e) e1 hhs
          Right regex -> do
            rr <- eval (Proxy @r) opts x
            pure $ case getValueLR NoInline opts msg0 rr hhs of
              Left e -> e
              Right r ->
               let ret :: String
                   ret = case q of
                           RReplace o s ->
                             let g fn = (if alle then RH.gsub else RH.sub) regex fn r
                             in g (case o of
                                  RPrepend -> (s <>)
                                  ROverWrite -> const s
                                  RAppend -> (<> s))
                           RReplace1 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace2 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace3 s -> (if alle then RH.gsub else RH.sub) regex s r
               in mkNode opts (Val ret) (msg1 <> " " <> litL opts r <> litVerbose opts " | " ret) (hhs <> [hh rr])

data ReplaceAll' (rs :: [ROpt]) p q r deriving Show
type ReplaceAllT' (rs :: [ROpt]) p q r = ReplaceImpl 'True rs p q r

instance P (ReplaceAllT' rs p q r) x => P (ReplaceAll' rs p q r) x where
  type PP (ReplaceAll' rs p q r) x = PP (ReplaceAllT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceAllT' rs p q r))

data ReplaceAll p q r deriving Show
type ReplaceAllT p q r = ReplaceAll' '[] p q r

instance P (ReplaceAllT p q r) x => P (ReplaceAll p q r) x where
  type PP (ReplaceAll p q r) x = PP (ReplaceAllT p q r) x
  eval _ = eval (Proxy @(ReplaceAllT p q r))

data ReplaceOne' (rs :: [ROpt]) p q r deriving Show
type ReplaceOneT' (rs :: [ROpt]) p q r = ReplaceImpl 'False rs p q r

instance P (ReplaceOneT' rs p q r) x => P (ReplaceOne' rs p q r) x where
  type PP (ReplaceOne' rs p q r) x = PP (ReplaceOneT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceOneT' rs p q r))

-- | replace first occurrence of string @p@ with @q@ in @r@
--
-- >>> pl @(ReplaceOneString 'ROverWrite "abc" "def" Id) "123abc456abc"
-- Present "123def456abc" (ReplaceOne (abc) 123abc456abc | 123def456abc)
-- Val "123def456abc"
--
-- >>> pz @(Rescan "^Date\\((\\d+[+-]\\d{4})\\)" >> Head >> Snd >> Id !! 0 >> ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id >> ParseTimeP ZonedTime "%s%Q%z") "Date(1530144000123+0530)"
-- Val 2018-06-28 05:30:00.123 +0530
--
-- >>> pz @(Rescan "^Date\\((\\d+[+-]\\d{4})\\)" >> Head >> Snd >> Id !! 0 >> ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id >> ParseTimeP ZonedTime "%s%Q%z") "Date(1593460089052+0800)"
-- Val 2020-06-30 03:48:09.052 +0800
--
-- >>> pz @(Rescan "^Date\\((\\d+)(\\d{3}[+-]\\d{4})\\)" >> Head >> Snd >> (Id !! 0 <> "." <> Id !! 1)  >> ParseTimeP ZonedTime "%s%Q%z") "Date(1593460089052+0800)"
-- Val 2020-06-30 03:48:09.052 +0800
--
data ReplaceOne p q r deriving Show
type ReplaceOneT p q r = ReplaceOne' '[] p q r

instance P (ReplaceOneT p q r) x => P (ReplaceOne p q r) x where
  type PP (ReplaceOne p q r) x = PP (ReplaceOneT p q r) x
  eval _ = eval (Proxy @(ReplaceOneT p q r))

-- | replace all occurrences of string @p@ with @q@ in @r@
--
-- >>> pl @(ReplaceAllString 'ROverWrite "abc" "def" Id) "123abc456abc"
-- Present "123def456def" (ReplaceAll (abc) 123abc456abc | 123def456def)
-- Val "123def456def"
--
-- >>> pl @(ReplaceAllString' '[] 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456def" (ReplaceAll (abc) 123AbC456abc | 123AbC456def)
-- Val "123AbC456def"
--
-- >>> pl @(ReplaceAllString' '[ 'Caseless ] 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123def456def" (ReplaceAll' ['Caseless] (abc) 123AbC456abc | 123def456def)
-- Val "123def456def"
--
-- >>> pl @(ReplaceAllString 'RPrepend "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456defabc" (ReplaceAll (abc) 123AbC456abc | 123AbC456defabc)
-- Val "123AbC456defabc"
--
-- >>> pl @(ReplaceAllString 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456def" (ReplaceAll (abc) 123AbC456abc | 123AbC456def)
-- Val "123AbC456def"
--
-- >>> pl @(ReplaceAllString 'RAppend "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456abcdef" (ReplaceAll (abc) 123AbC456abc | 123AbC456abcdef)
-- Val "123AbC456abcdef"
--
data ReplaceAllString' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r deriving Show
type ReplaceAllStringT' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r = ReplaceAll' rs p (ReplaceFn o q) r

instance P (ReplaceAllStringT' rs o p q r) x => P (ReplaceAllString' rs o p q r) x where
  type PP (ReplaceAllString' rs o p q r) x = PP (ReplaceAllStringT' rs o p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT' rs o p q r))

data ReplaceAllString o p q r deriving Show
type ReplaceAllStringT o p q r = ReplaceAllString' '[] o p q r

instance P (ReplaceAllStringT o p q r) x => P (ReplaceAllString o p q r) x where
  type PP (ReplaceAllString o p q r) x = PP (ReplaceAllStringT o p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT o p q r))

data ReplaceOneString' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r deriving Show
type ReplaceOneStringT' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r = ReplaceOne' rs p (ReplaceFn o q) r

instance P (ReplaceOneStringT' rs o p q r) x => P (ReplaceOneString' rs o p q r) x where
  type PP (ReplaceOneString' rs o p q r) x = PP (ReplaceOneStringT' rs o p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT' rs o p q r))

data ReplaceOneString (o :: ReplaceFnSub) p q r deriving Show
type ReplaceOneStringT (o :: ReplaceFnSub) p q r = ReplaceOneString' '[] o p q r

instance P (ReplaceOneStringT o p q r) x => P (ReplaceOneString o p q r) x where
  type PP (ReplaceOneString o p q r) x = PP (ReplaceOneStringT o p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT o p q r))

-- | Simple replacement string: see 'ReplaceAllString' and 'ReplaceOneString'
--
data ReplaceFn (o :: ReplaceFnSub) p deriving Show

instance ( GetReplaceFnSub r
         , PP p x ~ String
         , P p x
         ) => P (ReplaceFn r p) x where
  type PP (ReplaceFn r p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = RReplace (getReplaceFnSub @r) p
        in mkNode opts (Val b) (msg0 <> showVerbose opts " | " p) [hh pp]

-- | A replacement function @(String -> [String] -> String)@ which returns the whole match and the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
data ReplaceFn1 p deriving Show

instance ( PP p x ~ (String -> [String] -> String)
         , P p x
         ) => P (ReplaceFn1 p) x where
  type PP (ReplaceFn1 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn1 (String -> [String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (Val (RReplace1 f)) msg0 [hh pp]

-- | A replacement function @(String -> String)@ that yields the whole match
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(ReplaceAll "\\." (ReplaceFn2 Fst) Snd) (\x -> x <> ":" <> x, "141.201.1.22")
-- Val "141.:.201.:.1.:.22"
--
data ReplaceFn2 p deriving Show

instance ( PP p x ~ (String -> String)
         , P p x
         ) => P (ReplaceFn2 p) x where
  type PP (ReplaceFn2 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn2 (String -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (Val (RReplace2 f)) msg0 [hh pp]

-- | A replacement function @([String] -> String)@ which yields the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> import Data.List (intercalate)
-- >>> pz @(ReplaceAll "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" (ReplaceFn3 Fst) Snd) (\ys -> intercalate  " | " $ map (show . succ . readNote @Int "invalid int") ys, "141.201.1.22")
-- Val "142 | 202 | 2 | 23"
--
data ReplaceFn3 p deriving Show

instance ( PP p x ~ ([String] -> String)
         , P p x
         ) => P (ReplaceFn3 p) x where
  type PP (ReplaceFn3 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn3 ([String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR NoInline opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (Val (RReplace3 f)) msg0 [hh pp]
