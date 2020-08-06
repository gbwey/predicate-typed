{-# OPTIONS -Wall #-}
{-# OPTIONS -Wno-compat #-}
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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted regular expression functions
-}
module Predicate.Data.Regex (

  -- ** regex expressions
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
import Predicate.Util
import Data.Proxy
import qualified Text.Regex.PCRE.Heavy as RH

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import Safe (readNote)
-- >>> import Predicate.Prelude
-- >>> import Data.Time

-- | runs a regular expression with given regex options and returns a boolean: see 'RH.=~'
--
-- >>> pl @(Re' '[ 'Caseless, 'Dotall ] "ab" Id) "aB"
-- True (Re' ['Caseless, 'Dotall] (ab) | aB)
-- TrueT
--
-- >>> pl @(Re' '[ 'Caseless, 'Dotall ] "ab." Id) "aB\n"
-- True (Re' ['Caseless, 'Dotall] (ab.) | aB
-- )
-- TrueT
--
-- >>> pl @(Re' '[ 'Caseless ] "ab." Id) "aB\n"
-- False (Re' ['Caseless] (ab.) | aB
-- )
-- FalseT
--
data Re' (rs :: [ROpt]) p q

-- | runs a regular expression and returns a boolean: see 'RH.=~'
--
-- >>> pz @(Re "^\\d{2}:\\d{2}:\\d{2}$" Id) "13:05:25"
-- TrueT
--
-- >>> pl @(Re "\\d{4}-\\d{3}" Id) "1234-123"
-- True (Re (\d{4}-\d{3}) | 1234-123)
-- TrueT
--
-- >>> pl @(Re "\\d{4}-\\d{3}" Id) "1234-1x3"
-- False (Re (\d{4}-\d{3}) | 1234-1x3)
-- FalseT
--
-- >>> pl @(Re "(?i)ab" Id) "aB" -- runtime [use 'Caseless instead]
-- True (Re ((?i)ab) | aB)
-- TrueT
--
-- >>> pl @(Re "ab" Id) "aB"
-- False (Re (ab) | aB)
-- FalseT
--
-- >>> pl @(Re "^\\d{1,3}(?:\\.\\d{1,3}){3}$" Id) "123.1.1.21"
-- True (Re (^\d{1,3}(?:\.\d{1,3}){3}$) | 123.1.1.21)
-- TrueT
--
-- >>> pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$" Id) >> ReadP Double Id) "13.345"
-- Present 13.345 ((>>) 13.345 | {ReadP Double 13.345})
-- PresentT 13.345
--
-- >>> pl @(Guard "regex failed" (Re "^\\d+(?:\\.\\d+)?$" Id) >> ReadP Double Id) "13"
-- Present 13.0 ((>>) 13.0 | {ReadP Double 13.0})
-- PresentT 13.0
--
-- >>> pl @(ExitWhen "regex failed" (Not (Re "^\\d+(?:\\.\\d+)?$" Id)) >> ReadP Double Id) "-13.4"
-- Error regex failed ((>>) lhs failed)
-- FailT "regex failed"
--
-- >>> pl @(Re "\\d{4}\\" Id) "ayx"
-- Error Regex failed to compile (Re (\d{4}\) ([],[]):\ at end of pattern)
-- FailT "Regex failed to compile"
--
-- >>> pl @(Re "^\\d+$" Id) "123\nx"
-- False (Re (^\d+$) | 123
-- x)
-- FalseT
--
-- >>> pl @(Re "(?m)^\\d+$" Id) "123\nx" -- (?m) anchors match beginning/end of line instead of whole string
-- True (Re ((?m)^\d+$) | 123
-- x)
-- TrueT
--
-- >>> pl @(Catch (Re "\\d+(" Id) 'False) "123"
-- False (Catch caught exception[Regex failed to compile])
-- FalseT
--
-- >>> pl @(Catch (Re "\\d+" Id) 'False) "123"
-- True (Catch did not fire)
-- TrueT
--
data Re p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Re' rs p q) x where
  type PP (Re' rs p q) x = Bool
  eval _ opts x = do
    let msg0 = "Re" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
            Left tta -> tta
            Right regex ->
               let b = q RH.=~ regex
               in mkNodeB opts b (msg1 <> litVerbose opts " | " q) hhs

type ReT p q = Re' '[] p q

instance P (ReT p q) x => P (Re p q) x where
  type PP (Re p q) x = PP (ReT p q) x
  eval _ = evalBool (Proxy @(ReT p q))

-- only way with rescan is to be explicit: no repeats! and useanchors but not (?m)
-- or just use Re' but then we only get a bool ie doesnt capture groups
-- rescan returns Right [] as an failure!
-- [] is failure!
--  anchored means it has to start at the beginning: can have junk on the end which we cant detect but at least we know it starts at beginning


-- | runs a regex matcher returning the original values and optionally any groups: see 'RH.scan'
--
-- >>> pl @(Rescan' '[ 'Anchored ] "([[:xdigit:]]{2})" Id) "wfeb12az"
-- Error Regex no results (Rescan' ['Anchored] (([[:xdigit:]]{2})) | "wfeb12az")
-- FailT "Regex no results"
--
data Rescan' (rs :: [ROpt]) p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Rescan' rs p q) x where
  type PP (Rescan' rs p q) x = [(String, [String])]
  eval _ opts x = do
    let msg0 = "Rescan" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt (oRecursion opts) $ RH.scan regex q of
              (b, _:_) -> mkNode opts (FailT ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") (msg1 <> showVerbose opts " | " q) [hh pp, hh qq]
              (b, _) -> mkNode opts (PresentT b) (lit01 opts msg1 b "" q) [hh pp, hh qq]

-- | see 'RH.scan'
--
-- >>> pz @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
-- PresentT [("13:05:25",["13","05","25"])]
--
-- >>> pz @(Rescan (Snd Id) "13:05:25") ('a',"^(\\d{2}):(\\d{2}):(\\d{2})$")
-- PresentT [("13:05:25",["13","05","25"])]
--
-- >>> pz @(Rescan "^(\\d{2}):(\\d{2}):(\\d{2})$" Id >> Snd (Head Id) >> Map (ReadP Int Id) Id) "13:05:25"
-- PresentT [13,5,25]
--
-- >>> pl @(Rescan "(\\d+)\\D?" Id >> Map (Second (ReadP Int (OneP Id))) Id) "123-444-987"
-- Present [("123-",123),("444-",444),("987",987)] ((>>) [("123-",123),("444-",444),("987",987)] | {Map [("123-",123),("444-",444),("987",987)] | [("123-",["123"]),("444-",["444"]),("987",["987"])]})
-- PresentT [("123-",123),("444-",444),("987",987)]
--
-- >>> pl @(Rescan ".(.)" Id) "aBcd"
-- Present [("aB",["B"]),("cd",["d"])] (Rescan (.(.)) [("aB",["B"]),("cd",["d"])] | aBcd)
-- PresentT [("aB",["B"]),("cd",["d"])]
--
-- >>> pl @(Rescan "\\d{1,3}(\\.)?" Id) "123.8.99.21"
-- Present [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])] (Rescan (\d{1,3}(\.)?) [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])] | 123.8.99.21)
-- PresentT [("123.",["."]),("8.",["."]),("99.",["."]),("21",[])]
--
-- >>> pl @(Map (Fst Id) (Rescan "." (ShowP Id)) >> Filter (Same "2") Id) 12324
-- Present ["2","2"] ((>>) ["2","2"] | {Fst ["2","2"] | (["2","2"],["1","3","4"])})
-- PresentT ["2","2"]
--
-- >>> pl @(Rescan "(\\d)+?" Id) "1234"
-- Present [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])] (Rescan ((\d)+?) [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])] | 1234)
-- PresentT [("1",["1"]),("2",["2"]),("3",["3"]),("4",["4"])]
--
-- >>> pl @(Rescan "(\\d)+" Id) "1234"
-- Present [("1234",["4"])] (Rescan ((\d)+) [("1234",["4"])] | 1234)
-- PresentT [("1234",["4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?" Id) "1.2.3.4" -- overcapturing
-- Present [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] (Rescan ((\d{1,3})(\.(\d{1,3}))+?) [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] | 1.2.3.4)
-- PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]
--
-- >>> pl @(Rescan "^(\\d)+?$" Id) "1234"
-- Present [("1234",["4"])] (Rescan (^(\d)+?$) [("1234",["4"])] | 1234)
-- PresentT [("1234",["4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(\\.(\\d{1,3}))+?" Id) "1.2.3.4"
-- Present [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] (Rescan ((\d{1,3})(\.(\d{1,3}))+?) [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])] | 1.2.3.4)
-- PresentT [("1.2",["1",".2","2"]),("3.4",["3",".4","4"])]
--
-- >>> pl @(Rescan "(\\d{1,3})(?:\\.(\\d{1,3}))+?" Id) "1.2.3.4" -- bizzare!
-- Present [("1.2",["1","2"]),("3.4",["3","4"])] (Rescan ((\d{1,3})(?:\.(\d{1,3}))+?) [("1.2",["1","2"]),("3.4",["3","4"])] | 1.2.3.4)
-- PresentT [("1.2",["1","2"]),("3.4",["3","4"])]
--
-- >>> pl @(Rescan "^(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$" Id) "1.2.3.4"
-- Present [("1.2.3.4",["1","2","3","4"])] (Rescan (^(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$) [("1.2.3.4",["1","2","3","4"])] | 1.2.3.4)
-- PresentT [("1.2.3.4",["1","2","3","4"])]
--
-- >>> pl @(Rescan "([[:xdigit:]]{2})" Id) "wfeb12az"
-- Present [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])] (Rescan (([[:xdigit:]]{2})) [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])] | wfeb12az)
-- PresentT [("fe",["fe"]),("b1",["b1"]),("2a",["2a"])]
--
data Rescan p q
type RescanT p q = Rescan' '[] p q

instance P (RescanT p q) x => P (Rescan p q) x where
  type PP (Rescan p q) x = PP (RescanT p q) x
  eval _ = eval (Proxy @(RescanT p q))


-- | see 'RH.scanRanges'
--
-- >>> pz @(RescanRanges "^(\\d{2}):(\\d{2}):(\\d{2})$" Id) "13:05:25"
-- PresentT [((0,8),[(0,2),(3,5),(6,8)])]
--
data RescanRanges' (rs :: [ROpt]) p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (RescanRanges' rs p q) x where
  type PP (RescanRanges' rs p q) x = [((Int,Int), [(Int,Int)])]
  eval _ opts x = do
    let msg0 = "RescanRanges" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt (oRecursion opts) $ RH.scanRanges regex q of
              (b, _:_) -> mkNode opts (FailT ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") (msg1 <> showVerbose opts " | " q) hhs
              (b, _) -> mkNode opts (PresentT b) (lit01 opts msg1 b "" q) hhs

data RescanRanges p q
type RescanRangesT p q = RescanRanges' '[] p q

instance P (RescanRangesT p q) x => P (RescanRanges p q) x where
  type PP (RescanRanges p q) x = PP (RescanRangesT p q) x
  eval _ = eval (Proxy @(RescanRangesT p q))

-- | splits a string on a regex delimiter: see 'RH.split'
--
-- >>> pl @(Resplit' '[ 'Caseless ] "aBc" Id) "123AbC456abc"
-- Present ["123","456",""] (Resplit' ['Caseless] (aBc) ["123","456",""] | 123AbC456abc)
-- PresentT ["123","456",""]
--
data Resplit' (rs :: [ROpt]) p q

instance (GetROpts rs
        , PP p x ~ String
        , PP q x ~ String
        , P p x
        , P q x
        ) => P (Resplit' rs p q) x where
  type PP (Resplit' rs p q) x = [String]
  eval _ opts x = do
    let msg0 = "Resplit" <> unlessNull rs ("' " <> displayROpts fs)
        (fs,rs) = getROpts @rs
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case compileRegex @rs opts msg1 p hhs of
          Left tta -> tta
          Right regex ->
            case splitAt (oRecursion opts) $ RH.split regex q of
              (b, _:_) -> mkNode opts (FailT ("Regex looping(" ++ show (oRecursion opts) ++ ")")) (msg1 <> " " <> show (take 10 b) <> "..." <> showVerbose opts " | " q) hhs
              ([], _) -> -- this is a failure cos empty string returned: so reuse p?
                         mkNode opts (FailT "Regex no results") (msg1 <> showVerbose opts " | " q) hhs
              (b, _) -> mkNode opts (PresentT b) (lit01 opts msg1 b "" q) hhs

-- | splits a string on a regex delimiter: see 'RH.split'
--
-- >>> pz @(Resplit "\\." Id) "141.201.1.22"
-- PresentT ["141","201","1","22"]
--
-- >>> pz @(Resplit (Singleton (Fst Id)) (Snd Id)) (':', "12:13:1")
-- PresentT ["12","13","1"]
--
-- >>> pl @(Resplit "\\." Id) "123.2.3.5.6"
-- Present ["123","2","3","5","6"] (Resplit (\.) ["123","2","3","5","6"] | 123.2.3.5.6)
-- PresentT ["123","2","3","5","6"]
--
-- >>> pl @(Map (ReadP Int Id) (Resplit "\\." Id) >> '(Id, '(Len == 4, All (Between 0 255 Id) Id))) "141.214.125.1.2.3333"
-- Present ([141,214,125,1,2,3333],(False,False)) ((>>) ([141,214,125,1,2,3333],(False,False)) | {'([141,214,125,1,2,3333],(False,False))})
-- PresentT ([141,214,125,1,2,3333],(False,False))
--
-- >>> pl @(Map (ReadP Int Id) (Resplit "\\." Id) >> Id &&& ((Len == 4) &&& All (Between 0 255 Id) Id)) "141.214.125.1.2.6"
-- Present ([141,214,125,1,2,6],(False,True)) ((>>) ([141,214,125,1,2,6],(False,True)) | {W '([141,214,125,1,2,6],(False,True))})
-- PresentT ([141,214,125,1,2,6],(False,True))
--
-- >>> pl @(Resplit "\\." Id >> Map (ReadP Int Id) Id >> Id &&& ((Len == 4) &&& All (Between 0 255 Id) Id)) "141.214.125."
-- Error ReadP Int () (["141","214","125",""] (>>) rhs failed)
-- FailT "ReadP Int ()"
--
data Resplit p q
type ResplitT p q = Resplit' '[] p q

instance P (ResplitT p q) x => P (Resplit p q) x where
  type PP (Resplit p q) x = PP (ResplitT p q) x
  eval _ = eval (Proxy @(ResplitT p q))

-- | replaces regex \'s\' with a string \'s1\' inside the value: see 'RH.sub' and 'RH.gsub'
--
-- >>> pz @(ReplaceAllString 'ROverWrite "\\." ":" Id) "141.201.1.22"
-- PresentT "141:201:1:22"
--
data ReplaceImpl (alle :: Bool) (rs :: [ROpt]) p q r

instance (GetBool b
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
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
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
                           RReplace o s ->
                             let g fn = (if alle then RH.gsub else RH.sub) regex fn r
                             in g (case o of
                                  RPrepend -> (s <>)
                                  ROverWrite -> const s
                                  RAppend -> (<> s))
                           RReplace1 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace2 s -> (if alle then RH.gsub else RH.sub) regex s r
                           RReplace3 s -> (if alle then RH.gsub else RH.sub) regex s r
               in mkNode opts (PresentT ret) (msg1 <> " " <> litL opts r <> litVerbose opts " | " ret) (hhs <> [hh rr])

data ReplaceAll' (rs :: [ROpt]) p q r
type ReplaceAllT' (rs :: [ROpt]) p q r = ReplaceImpl 'True rs p q r

instance P (ReplaceAllT' rs p q r) x => P (ReplaceAll' rs p q r) x where
  type PP (ReplaceAll' rs p q r) x = PP (ReplaceAllT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceAllT' rs p q r))

data ReplaceAll p q r
type ReplaceAllT p q r = ReplaceAll' '[] p q r

instance P (ReplaceAllT p q r) x => P (ReplaceAll p q r) x where
  type PP (ReplaceAll p q r) x = PP (ReplaceAllT p q r) x
  eval _ = eval (Proxy @(ReplaceAllT p q r))

data ReplaceOne' (rs :: [ROpt]) p q r
type ReplaceOneT' (rs :: [ROpt]) p q r = ReplaceImpl 'False rs p q r

instance P (ReplaceOneT' rs p q r) x => P (ReplaceOne' rs p q r) x where
  type PP (ReplaceOne' rs p q r) x = PP (ReplaceOneT' rs p q r) x
  eval _ = eval (Proxy @(ReplaceOneT' rs p q r))

-- | replace first occurrence of string \'p\' with '\q'\ in \'r\'
--
-- >>> pl @(ReplaceOneString 'ROverWrite "abc" "def" Id) "123abc456abc"
-- Present "123def456abc" (ReplaceOne (abc) 123abc456abc | 123def456abc)
-- PresentT "123def456abc"
--
-- >>> pz @(Rescan "^Date\\((\\d+[+-]\\d{4})\\)" Id >> Head Id >> Snd Id >> Id !! 0 >> ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id >> ParseTimeP ZonedTime "%s%Q%z" Id) "Date(1530144000123+0530)"
-- PresentT 2018-06-28 05:30:00.123 +0530
--
-- >>> pz @(Rescan "^Date\\((\\d+[+-]\\d{4})\\)" Id >> Head Id >> Snd Id >> Id !! 0 >> ReplaceOneString 'RPrepend "\\d{3}[+-]" "." Id >> ParseTimeP ZonedTime "%s%Q%z" Id) "Date(1593460089052+0800)"
-- PresentT 2020-06-30 03:48:09.052 +0800
--
-- >>> pz @(Rescan "^Date\\((\\d+)(\\d{3}[+-]\\d{4})\\)" Id >> Head Id >> Snd Id >> (Id !! 0 <> "." <> Id !! 1)  >> ParseTimeP ZonedTime "%s%Q%z" Id) "Date(1593460089052+0800)"
-- PresentT 2020-06-30 03:48:09.052 +0800
--
data ReplaceOne p q r
type ReplaceOneT p q r = ReplaceOne' '[] p q r

instance P (ReplaceOneT p q r) x => P (ReplaceOne p q r) x where
  type PP (ReplaceOne p q r) x = PP (ReplaceOneT p q r) x
  eval _ = eval (Proxy @(ReplaceOneT p q r))

-- | replace all occurrences of string \'p\' with '\q'\ in \'r\'
--
-- >>> pl @(ReplaceAllString 'ROverWrite "abc" "def" Id) "123abc456abc"
-- Present "123def456def" (ReplaceAll (abc) 123abc456abc | 123def456def)
-- PresentT "123def456def"
--
-- >>> pl @(ReplaceAllString' '[] 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456def" (ReplaceAll (abc) 123AbC456abc | 123AbC456def)
-- PresentT "123AbC456def"
--
-- >>> pl @(ReplaceAllString' '[ 'Caseless ] 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123def456def" (ReplaceAll' ['Caseless] (abc) 123AbC456abc | 123def456def)
-- PresentT "123def456def"
--
-- >>> pl @(ReplaceAllString 'RPrepend "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456defabc" (ReplaceAll (abc) 123AbC456abc | 123AbC456defabc)
-- PresentT "123AbC456defabc"
--
-- >>> pl @(ReplaceAllString 'ROverWrite "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456def" (ReplaceAll (abc) 123AbC456abc | 123AbC456def)
-- PresentT "123AbC456def"
--
-- >>> pl @(ReplaceAllString 'RAppend "abc" "def" Id) "123AbC456abc"
-- Present "123AbC456abcdef" (ReplaceAll (abc) 123AbC456abc | 123AbC456abcdef)
-- PresentT "123AbC456abcdef"
--
data ReplaceAllString' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r
type ReplaceAllStringT' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r = ReplaceAll' rs p (ReplaceFn o q) r

instance P (ReplaceAllStringT' rs o p q r) x => P (ReplaceAllString' rs o p q r) x where
  type PP (ReplaceAllString' rs o p q r) x = PP (ReplaceAllStringT' rs o p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT' rs o p q r))

data ReplaceAllString o p q r
type ReplaceAllStringT o p q r = ReplaceAllString' '[] o p q r

instance P (ReplaceAllStringT o p q r) x => P (ReplaceAllString o p q r) x where
  type PP (ReplaceAllString o p q r) x = PP (ReplaceAllStringT o p q r) x
  eval _ = eval (Proxy @(ReplaceAllStringT o p q r))

data ReplaceOneString' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r
type ReplaceOneStringT' (rs :: [ROpt]) (o :: ReplaceFnSub) p q r = ReplaceOne' rs p (ReplaceFn o q) r

instance P (ReplaceOneStringT' rs o p q r) x => P (ReplaceOneString' rs o p q r) x where
  type PP (ReplaceOneString' rs o p q r) x = PP (ReplaceOneStringT' rs o p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT' rs o p q r))

data ReplaceOneString (o :: ReplaceFnSub) p q r
type ReplaceOneStringT (o :: ReplaceFnSub) p q r = ReplaceOneString' '[] o p q r

instance P (ReplaceOneStringT o p q r) x => P (ReplaceOneString o p q r) x where
  type PP (ReplaceOneString o p q r) x = PP (ReplaceOneStringT o p q r) x
  eval _ = eval (Proxy @(ReplaceOneStringT o p q r))

-- | Simple replacement string: see 'ReplaceAllString' and 'ReplaceOneString'
--
data ReplaceFn (o :: ReplaceFnSub) p

instance (GetReplaceFnSub r
        , PP p x ~ String
        , P p x) => P (ReplaceFn r p) x where
  type PP (ReplaceFn r p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let b = RReplace (getReplaceFnSub @r) p
        in mkNode opts (PresentT b) (msg0 <> showVerbose opts " | " p) [hh pp]

-- | A replacement function @(String -> [String] -> String)@ which returns the whole match and the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
data ReplaceFn1 p

instance (PP p x ~ (String -> [String] -> String)
        , P p x) => P (ReplaceFn1 p) x where
  type PP (ReplaceFn1 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn1 (String -> [String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace1 f)) msg0 [hh pp]

-- | A replacement function @(String -> String)@ that yields the whole match
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> pz @(ReplaceAll "\\." (ReplaceFn2 (Fst Id)) (Snd Id)) (\x -> x <> ":" <> x, "141.201.1.22")
-- PresentT "141.:.201.:.1.:.22"
--
data ReplaceFn2 p

instance (PP p x ~ (String -> String)
        , P p x) => P (ReplaceFn2 p) x where
  type PP (ReplaceFn2 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn2 (String -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace2 f)) msg0 [hh pp]

-- | A replacement function @([String] -> String)@ which yields the groups
-- Used by 'RH.sub' and 'RH.gsub'
--
-- Requires "Text.Show.Functions"
--
-- >>> :m + Text.Show.Functions
-- >>> import Data.List (intercalate)
-- >>> pz @(ReplaceAll "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$" (ReplaceFn3 (Fst Id)) (Snd Id)) (\ys -> intercalate  " | " $ map (show . succ . readNote @Int "invalid int") ys, "141.201.1.22")
-- PresentT "142 | 202 | 2 | 23"
--
data ReplaceFn3 p

instance (PP p x ~ ([String] -> String)
        , P p x) => P (ReplaceFn3 p) x where
  type PP (ReplaceFn3 p) x = RReplace
  eval _ opts x = do
    let msg0 = "ReplaceFn3 ([String] -> String)"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right f -> mkNode opts (PresentT (RReplace3 f)) msg0 [hh pp]