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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted date time functions
-}
module Predicate.Data.DateTime (

  -- ** format
    FormatTimeP

  -- ** constructors
  , ParseTimeP
  , ParseTimeP'
  , ParseTimes
  , ParseTimes'
  , MkDay
  , MkDay'
  , MkDayExtra
  , MkDayExtra'
  , MkTime
  , MkTime'
  , PosixToUTCTime

 -- ** destructors
  , UnMkDay
  , ToWeekDate
  , ToWeekYear
  , ToDay
  , ToTime
  , UnMkTime
  , UTCTimeToPosix

 ) where
import Predicate.Core
import Predicate.Util
import Control.Lens hiding (iall)
import Data.Proxy
import Data.Typeable
import Data.Kind (Type)
import Data.Maybe
import Data.Time
import Data.Time.Calendar.WeekDate
import qualified Data.Time.Clock.System as CP
import qualified Data.Time.Clock.POSIX as P
-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeApplications
-- >>> :set -XTypeOperators
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoOverloadedLists
-- >>> import qualified Data.Map.Strict as M
-- >>> import qualified Data.Set as Set
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import qualified Data.Semigroup as SG
-- >>> import Safe (readNote)

-- | type level expression representing a formatted time
-- similar to 'Data.Time.formatTime' using a type level 'GHC.TypeLits.Symbol' to get the formatting string
--
-- >>> pz @(FormatTimeP "%F %T" Id) (readNote @LocalTime "invalid localtime" "2019-05-24 05:19:59")
-- PresentT "2019-05-24 05:19:59"
--
-- >>> pz @(FormatTimeP (Fst Id) (Snd Id)) ("the date is %d/%m/%Y", readNote @Day "invalid day" "2019-05-24")
-- PresentT "the date is 24/05/2019"
--
-- >>> pl @(FormatTimeP "%Y-%m-%d" Id) (readNote @Day "invalid day" "2019-08-17")
-- Present "2019-08-17" (FormatTimeP (%Y-%m-%d) 2019-08-17 | 2019-08-17)
-- PresentT "2019-08-17"
--
data FormatTimeP p q

instance (PP p x ~ String
        , FormatTime (PP q x)
        , P p x
        , Show (PP q x)
        , P q x
        ) => P (FormatTimeP p q) x where
  type PP (FormatTimeP p q) x = String
  eval _ opts x = do
    let msg0 = "FormatTimeP"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            b = formatTime defaultTimeLocale p q
        in mkNode opts (PresentT b) (msg1 <> " " <> litL opts b <> showVerbose opts " | " q) [hh pp, hh qq]

-- | similar to 'Data.Time.parseTimeM' where \'t\' is the 'Data.Time.ParseTime' type, \'p\' is the datetime format and \'q\' points to the content to parse
--
-- >>> pz @(ParseTimeP LocalTime "%F %T" Id) "2019-05-24 05:19:59"
-- PresentT 2019-05-24 05:19:59
--
-- >>> pz @(ParseTimeP LocalTime "%F %T" "2019-05-24 05:19:59") (Right "never used")
-- PresentT 2019-05-24 05:19:59
--
-- keeping \'q\' as we might want to extract from a tuple
data ParseTimeP' t p q

instance (ParseTime (PP t a)
        , Typeable (PP t a)
        , Show (PP t a)
        , P p a
        , P q a
        , PP p a ~ String
        , PP q a ~ String
        ) => P (ParseTimeP' t p q) a where
  type PP (ParseTimeP' t p q) a = PP t a
  eval _ opts a = do
    let msg0 = "ParseTimeP " <> t
        t = showT @(PP t a)
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            hhs = [hh pp, hh qq]
        in case parseTimeM @Maybe @(PP t a) True defaultTimeLocale p q of
             Just b -> mkNode opts (PresentT b) (lit01 opts msg1 b "fmt=" p <> showVerbose opts " | " q) hhs
             Nothing -> mkNode opts (FailT (msg1 <> " failed to parse")) "" hhs
-- | parse time
--
-- >>> pl @(ParseTimeP TimeOfDay "%H:%M%S" Id) "14:04:61"
-- Error ParseTimeP TimeOfDay (%H:%M%S) failed to parse
-- FailT "ParseTimeP TimeOfDay (%H:%M%S) failed to parse"
--
-- >>> pl @(ParseTimeP UTCTime "%F %T" Id) "1999-01-01 12:12:12"
-- Present 1999-01-01 12:12:12 UTC (ParseTimeP UTCTime (%F %T) 1999-01-01 12:12:12 UTC | fmt=%F %T | "1999-01-01 12:12:12")
-- PresentT 1999-01-01 12:12:12 UTC
--

data ParseTimeP (t :: Type) p q
type ParseTimePT (t :: Type) p q = ParseTimeP' (Hole t) p q

instance P (ParseTimePT t p q) x => P (ParseTimeP t p q) x where
  type PP (ParseTimeP t p q) x = PP (ParseTimePT t p q) x
  eval _ = eval (Proxy @(ParseTimePT t p q))

-- | A convenience method to match against many different datetime formats to find a match
--
-- >>> pz @(ParseTimes LocalTime '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"] "03/11/19 01:22:33") ()
-- PresentT 2019-03-11 01:22:33
--
-- >>> pz @(ParseTimes LocalTime (Fst Id) (Snd Id)) (["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"], "03/11/19 01:22:33")
-- PresentT 2019-03-11 01:22:33
--
-- >>> pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id) Id) ["2001-01-01", "Jan 24 2009", "03/29/0x7"]
-- Error no match on (03/29/0x7) (Map(i=2, a="03/29/0x7") excnt=1)
-- FailT "no match on (03/29/0x7)"
--
-- >>> pl @(Map (ParseTimes Day '["%Y-%m-%d", "%m/%d/%y", "%b %d %Y"] Id) Id) ["2001-01-01", "Jan 24 2009", "03/29/07"]
-- Present [2001-01-01,2009-01-24,2007-03-29] (Map [2001-01-01,2009-01-24,2007-03-29] | ["2001-01-01","Jan 24 2009","03/29/07"])
-- PresentT [2001-01-01,2009-01-24,2007-03-29]
--
data ParseTimes' t p q

instance (ParseTime (PP t a)
        , Typeable (PP t a)
        , Show (PP t a)
        , P p a
        , P q a
        , PP p a ~ [String]
        , PP q a ~ String
        ) => P (ParseTimes' t p q) a where
  type PP (ParseTimes' t p q) a = PP t a
  eval _ opts a = do
    let msg0 = "ParseTimes " <> t
        t = showT @(PP t a)
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts a []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let hhs = [hh pp, hh qq]
            zs = map (\d -> (d,) <$> parseTimeM @Maybe @(PP t a) True defaultTimeLocale d q) p
        in case catMaybes zs of
             [] -> mkNode opts (FailT ("no match on (" ++ q ++ ")")) msg0 hhs
             (d,b):_ -> mkNode opts (PresentT b) (lit01 opts msg0 b "fmt=" d <> showVerbose opts " | " q) hhs

data ParseTimes (t :: Type) p q
type ParseTimesT (t :: Type) p q = ParseTimes' (Hole t) p q

instance P (ParseTimesT t p q) x => P (ParseTimes t p q) x where
  type PP (ParseTimes t p q) x = PP (ParseTimesT t p q) x
  eval _ = eval (Proxy @(ParseTimesT t p q))

-- | create a 'Day' from three int values passed in as year month and day
--
-- >>> pz @(MkDay '(1,2,3) >> 'Just Id) ()
-- PresentT 0001-02-03
--
-- >>> pz @(Just (MkDay '(1,2,3))) 1
-- PresentT 0001-02-03
--
-- >>> pz @(MkDay Id) (2019,12,30)
-- PresentT (Just 2019-12-30)
--
-- >>> pz @(MkDay' (Fst Id) (Snd Id) (Thd Id)) (2019,99,99999)
-- PresentT Nothing
--
-- >>> pz @(MkDay Id) (1999,3,13)
-- PresentT (Just 1999-03-13)
--
data MkDay' p q r

instance (P p x
        , P q x
        , P r x
        , PP p x ~ Int
        , PP q x ~ Int
        , PP r x ~ Int
        ) => P (MkDay' p q r) x where
  type PP (MkDay' p q r) x = Maybe Day
  eval _ opts x = do
    let msg0 = "MkDay"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        rr <- eval (Proxy @r) opts x
        pure $ case getValueLR opts msg0 rr hhs of
          Left e -> e
          Right r ->
            let mday = fromGregorianValid (fromIntegral p) q r
            in mkNode opts (PresentT mday) (show01' opts msg0 mday "(y,m,d)=" (p,q,r)) (hhs <> [hh rr])

data MkDay p
type MkDayT p = MkDay' (Fst p) (Snd p) (Thd p)

instance P (MkDayT p) x => P (MkDay p) x where
  type PP (MkDay p) x = PP (MkDayT p) x
  eval _ = eval (Proxy @(MkDayT p))

-- | uncreate a 'Day' returning year month and day
--
-- >>> pz @(UnMkDay Id) (readNote "invalid day" "2019-12-30")
-- PresentT (2019,12,30)
--
data UnMkDay p

instance ( PP p x ~ Day
         , P p x
         ) => P (UnMkDay p) x where
  type PP (UnMkDay p) x = (Int, Int, Int)
  eval _ opts x = do
    let msg0 = "UnMkDay"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (fromIntegral -> y, m, d) = toGregorian p
            b = (y, m, d)
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]


-- | create a 'Day' + Week + Day of Week from three int values passed in as year month and day
--
-- >>> pz @(MkDayExtra '(1,2,3) >> 'Just Id >> Fst Id) ()
-- PresentT 0001-02-03
--
-- >>> pz @(Fst (Just (MkDayExtra '(1,2,3)))) 1
-- PresentT 0001-02-03
--
-- >>> pz @(MkDayExtra Id) (2019,12,30)
-- PresentT (Just (2019-12-30,1,1))
--
-- >>> pz @(MkDayExtra' (Fst Id) (Snd Id) (Thd Id)) (2019,99,99999)
-- PresentT Nothing
--
-- >>> pz @(MkDayExtra Id) (1999,3,13)
-- PresentT (Just (1999-03-13,10,6))
--
data MkDayExtra' p q r

instance (P p x
        , P q x
        , P r x
        , PP p x ~ Int
        , PP q x ~ Int
        , PP r x ~ Int
        ) => P (MkDayExtra' p q r) x where
  type PP (MkDayExtra' p q r) x = Maybe (Day, Int, Int)
  eval _ opts x = do
    let msg0 = "MkDayExtra"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        rr <- eval (Proxy @r) opts x
        pure $ case getValueLR opts msg0 rr hhs of
          Left e -> e
          Right r ->
            let mday = fromGregorianValid (fromIntegral p) q r
                b = mday <&> \day ->
                      let (_, week, dow) = toWeekDate day
                      in (day, week, dow)
            in mkNode opts (PresentT b) (show01' opts msg0 b "(y,m,d)=" (p,q,r)) (hhs <> [hh rr])

data MkDayExtra p
type MkDayExtraT p = MkDayExtra' (Fst p) (Snd p) (Thd p)

instance P (MkDayExtraT p) x => P (MkDayExtra p) x where
  type PP (MkDayExtra p) x = PP (MkDayExtraT p) x
  eval _ = eval (Proxy @(MkDayExtraT p))

-- | get day of week
--
-- >>> pz @(Just (MkDay '(2020,7,11)) >> '(UnMkDay Id, ToWeekYear Id,ToWeekDate Id)) ()
-- PresentT ((2020,7,11),28,(6,"Saturday"))
--
data ToWeekDate p

instance ( P p x
         , PP p x ~ Day
         ) => P (ToWeekDate p) x where
  type PP (ToWeekDate p) x = (Int, String)
  eval _ opts x = do
    let msg0 = "ToWeekDate"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (_, _week, dow) = toWeekDate p
            dowString =
              case dow of
                 1 -> "Monday"
                 2 -> "Tuesday"
                 3 -> "Wednesday"
                 4 -> "Thursday"
                 5 -> "Friday"
                 6 -> "Saturday"
                 7 -> "Sunday"
                 _ -> error $ "oops: ToWeekDate invalid " ++ show dow
        in mkNode opts (PresentT (dow,dowString)) (show01 opts msg0 dow p) [hh pp]

-- | get week number of the year
--
-- >>> pz @(Just (MkDay '(2020,7,11)) >> ToWeekYear Id) ()
-- PresentT 28
--
data ToWeekYear p

instance ( P p x
         , PP p x ~ Day
         ) => P (ToWeekYear p) x where
  type PP (ToWeekYear p) x = Int
  eval _ opts x = do
    let msg0 = "ToWeekYear"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let (_, week, _dow) = toWeekDate p
        in mkNode opts (PresentT week) (show01 opts msg0 week p) [hh pp]

class ToDayC a where
  getDay :: a -> Day
instance ToDayC UTCTime where
  getDay = utctDay
instance ToDayC ZonedTime where
  getDay = getDay . zonedTimeToLocalTime
instance ToDayC LocalTime where
  getDay = localDay
instance ToDayC Day where
  getDay = id
instance ToDayC Rational where
  getDay = getDay . P.posixSecondsToUTCTime . fromRational
instance ToDayC CP.SystemTime where
  getDay = getDay . CP.systemToUTCTime

class ToTimeC a where
  getTime :: a -> TimeOfDay
instance ToTimeC UTCTime where
  getTime = getTime . utctDayTime
instance ToTimeC ZonedTime where
  getTime = getTime . zonedTimeToLocalTime
instance ToTimeC LocalTime where
  getTime = localTimeOfDay
instance ToTimeC TimeOfDay where
  getTime = id
instance ToTimeC DiffTime where
  getTime = timeToTimeOfDay
instance ToTimeC Rational where
  getTime = getTime . P.posixSecondsToUTCTime . fromRational
instance ToTimeC CP.SystemTime where
  getTime = getTime . CP.systemToUTCTime

-- | extract 'Day' from a DateTime
--
-- >>> pz @(ReadP UTCTime Id >> ToDay Id) "2020-07-06 12:11:13Z"
-- PresentT 2020-07-06
--
data ToDay p

instance ( P p x
         , Show (PP p x)
         , ToDayC (PP p x)
         ) => P (ToDay p) x where
  type PP (ToDay p) x = Day
  eval _ opts x = do
    let msg0 = "ToDay"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let ret = getDay p
        in mkNode opts (PresentT ret) (show01 opts msg0 ret p) [hh pp]

-- | extract 'TimeOfDay' from DateTime
--
-- >>> pz @(ReadP UTCTime Id >> ToDay Id) "2020-07-06 12:11:13Z"
-- PresentT 2020-07-06
--
data ToTime p

instance ( P p x
         , Show (PP p x)
         , ToTimeC (PP p x)
         ) => P (ToTime p) x where
  type PP (ToTime p) x = TimeOfDay
  eval _ opts x = do
    let msg0 = "ToTime"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let ret = getTime p
        in mkNode opts (PresentT ret) (show01 opts msg0 ret p) [hh pp]


-- | create a 'TimeOfDay' from three int values passed in as year month and day
--
-- >>> pz @(MkTime' (Fst Id) (Snd Id) (Thd Id)) (13,99,99999)
-- PresentT 13:99:99999
--
data MkTime' p q r

instance (P p x
        , P q x
        , P r x
        , PP p x ~ Int
        , PP q x ~ Int
        , PP r x ~ Rational
        ) => P (MkTime' p q r) x where
  type PP (MkTime' p q r) x = TimeOfDay
  eval _ opts x = do
    let msg0 = "MkTime"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    case lr of
      Left e -> pure e
      Right (p,q,pp,qq) -> do
        let hhs = [hh pp, hh qq]
        rr <- eval (Proxy @r) opts x
        pure $ case getValueLR opts msg0 rr hhs of
          Left e -> e
          Right r ->
            let mtime = TimeOfDay p q (fromRational r)
            in mkNode opts (PresentT mtime) (show01' opts msg0 mtime "(h,m,s)=" (p,q,r)) (hhs <> [hh rr])

-- | create a 'TimeOfDay' from a three-tuple of year month and day
--
-- >>> pz @(MkTime '(1,2,3 % 12345)) ()
-- PresentT 01:02:00.000243013365
--
-- >>> pz @(MkTime Id) (12,13,65)
-- PresentT 12:13:65
--
-- >>> pz @(MkTime Id) (17,3,13)
-- PresentT 17:03:13
--
data MkTime p
type MkTimeT p = MkTime' (Fst p) (Snd p) (Thd p)

instance P (MkTimeT p) x => P (MkTime p) x where
  type PP (MkTime p) x = PP (MkTimeT p) x
  eval _ = eval (Proxy @(MkTimeT p))


-- | uncreate a 'TimeOfDay' returning hour minute seconds picoseconds
--
-- >>> pz @(ReadP UTCTime "2019-01-01 12:13:14.1234Z" >> ToTime Id >> UnMkTime Id) ()
-- PresentT (12,13,70617 % 5000)
--
-- >>> pz @(ReadP UTCTime Id >> ToTime Id >> UnMkTime Id) "2020-07-22 08:01:14.127Z"
-- PresentT (8,1,14127 % 1000)
--
-- >>> pz @(ReadP ZonedTime Id >> '(UnMkDay (ToDay Id), UnMkTime (ToTime Id))) "2020-07-11 11:41:12.333 CET"
-- PresentT ((2020,7,11),(11,41,12333 % 1000))
--
data UnMkTime p

instance ( PP p x ~ TimeOfDay
         , P p x
         ) => P (UnMkTime p) x where
  type PP (UnMkTime p) x = (Int, Int, Rational)
  eval _ opts x = do
    let msg0 = "UnMkTime"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let TimeOfDay h m s = p
            b = (h, m, toRational s)
        in mkNode opts (PresentT b) (show01 opts msg0 b p) [hh pp]


-- microsoft json date is x*1000 ie milliseconds

-- | convert posix time (seconds since 01-01-1970) to 'UTCTime'
--
-- >>> pl @(PosixToUTCTime Id) 1593384312
-- Present 2020-06-28 22:45:12 UTC (PosixToUTCTime 2020-06-28 22:45:12 UTC | 1593384312 % 1)
-- PresentT 2020-06-28 22:45:12 UTC
--
-- >>> pl @(PosixToUTCTime Id >> UTCTimeToPosix Id) 1593384312
-- Present 1593384312 % 1 ((>>) 1593384312 % 1 | {UTCTimeToPosix 1593384312 % 1 | 2020-06-28 22:45:12 UTC})
-- PresentT (1593384312 % 1)
--
-- >>> pl @(PosixToUTCTime (Id % 1000)) 1593384312000
-- Present 2020-06-28 22:45:12 UTC (PosixToUTCTime 2020-06-28 22:45:12 UTC | 1593384312 % 1)
-- PresentT 2020-06-28 22:45:12 UTC
--
-- >>> pl @(PosixToUTCTime Id) (3600*4+60*7+12)
-- Present 1970-01-01 04:07:12 UTC (PosixToUTCTime 1970-01-01 04:07:12 UTC | 14832 % 1)
-- PresentT 1970-01-01 04:07:12 UTC
--
-- >>> pz @(Rescan "^Date\\((\\d+)([^\\)]+)\\)" Id >> Head Id >> Snd Id >> ReadP Integer (Id !! 0) >> PosixToUTCTime (Id % 1000)) "Date(1530144000000+0530)"
-- PresentT 2018-06-28 00:00:00 UTC
--
data PosixToUTCTime p

instance ( PP p x ~ Rational
         , P p x
         ) => P (PosixToUTCTime p) x where
  type PP (PosixToUTCTime p) x = UTCTime
  eval _ opts x = do
    let msg0 = "PosixToUTCTime"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = P.posixSecondsToUTCTime (fromRational p)
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

-- | convert 'UTCTime' to posix time (seconds since 01-01-1970)
--
-- >>> pl @(ReadP UTCTime Id >> UTCTimeToPosix Id) "2020-06-28 22:45:12 UTC"
-- Present 1593384312 % 1 ((>>) 1593384312 % 1 | {UTCTimeToPosix 1593384312 % 1 | 2020-06-28 22:45:12 UTC})
-- PresentT (1593384312 % 1)
--
-- >>> pz @(Rescan "^Date\\((\\d+)([^\\)]+)\\)" Id >> Head Id >> Snd Id >> ((ReadP Integer (Id !! 0) >> PosixToUTCTime (Id % 1000)) &&& ReadP TimeZone (Id !! 1))) "Date(1530144000000+0530)"
-- PresentT (2018-06-28 00:00:00 UTC,+0530)
--
-- not so useful: instead use ParseTimeP FormatTimeP with %s %q %z etc
--
-- >>> pz @(ParseTimeP ZonedTime "%s%Q%z" Id)  "153014400.000+0530"
-- PresentT 1974-11-07 05:30:00 +0530
--
data UTCTimeToPosix p

instance ( PP p x ~ UTCTime
         , P p x
         ) => P (UTCTimeToPosix p) x where
  type PP (UTCTimeToPosix p) x = Rational
  eval _ opts x = do
    let msg0 = "UTCTimeToPosix"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = toRational $ P.utcTimeToPOSIXSeconds p
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

