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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoStarIsType #-}
{- |
     promoted date time functions
-}
module Predicate.Data.DateTime (

  -- ** format
    FormatTimeP
  , FormatTimeP'

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
  , DiffUTCTime
  , DiffLocalTime

 -- ** destructors
  , UnMkDay
  , ToWeekDate
  , ToWeekYear
  , ToDay
  , ToTime
  , UnMkTime
  , UTCTimeToPosix
  , LocalTimeToUTC

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
-- >>> import qualified Data.Text as T
-- >>> import Predicate.Prelude
-- >>> import Safe (readNote)

-- | type level expression representing a formatted time
--   similar to 'Data.Time.formatTime' using a type level 'GHC.TypeLits.Symbol' to get the formatting string
--
-- >>> pz @(FormatTimeP' Fst Snd) ("the date is %d/%m/%Y", readNote @Day "invalid day" "2019-05-24")
-- PresentT "the date is 24/05/2019"
--
data FormatTimeP' p q

instance (PP p x ~ String
        , FormatTime (PP q x)
        , P p x
        , Show (PP q x)
        , P q x
        ) => P (FormatTimeP' p q) x where
  type PP (FormatTimeP' p q) x = String
  eval _ opts x = do
    let msg0 = "FormatTimeP"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let msg1 = msg0 <> " (" <> p <> ")"
            b = formatTime defaultTimeLocale p q
        in mkNode opts (PresentT b) (msg1 <> " " <> litL opts b <> showVerbose opts " | " q) [hh pp, hh qq]

-- | type level expression representing a formatted time
--
-- >>> pz @(FormatTimeP "%F %T") (readNote @LocalTime "invalid localtime" "2019-05-24 05:19:59")
-- PresentT "2019-05-24 05:19:59"
--
-- >>> pl @(FormatTimeP "%Y-%m-%d") (readNote @Day "invalid day" "2019-08-17")
-- Present "2019-08-17" (FormatTimeP (%Y-%m-%d) 2019-08-17 | 2019-08-17)
-- PresentT "2019-08-17"
--
data FormatTimeP p
type FormatTimePT p = FormatTimeP' p Id

instance P (FormatTimePT p) x => P (FormatTimeP p) x where
  type PP (FormatTimeP p) x = PP (FormatTimePT p) x
  eval _ = eval (Proxy @(FormatTimePT p))



-- | similar to 'Data.Time.parseTimeM' where @t@ is the 'Data.Time.ParseTime' type, @p@ is the datetime format and @q@ points to the content to parse
-- keeping @q@ as we might want to extract from a tuple
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
-- | similar to 'Date.Time.parseTimeM'
--
-- >>> pz @(ParseTimeP LocalTime "%F %T") "2019-05-24 05:19:59"
-- PresentT 2019-05-24 05:19:59
--
-- >>> pz @("2019-05-24 05:19:59" >> ParseTimeP LocalTime "%F %T") (Right "never used")
-- PresentT 2019-05-24 05:19:59
--
-- >>> pl @(ParseTimeP TimeOfDay "%H:%M%S") "14:04:61"
-- Error ParseTimeP TimeOfDay (%H:%M%S) failed to parse
-- FailT "ParseTimeP TimeOfDay (%H:%M%S) failed to parse"
--
-- >>> pl @(ParseTimeP UTCTime "%F %T") "1999-01-01 12:12:12"
-- Present 1999-01-01 12:12:12 UTC (ParseTimeP UTCTime (%F %T) 1999-01-01 12:12:12 UTC | fmt=%F %T | "1999-01-01 12:12:12")
-- PresentT 1999-01-01 12:12:12 UTC
--
-- >>> pz @(ParseTimeP ZonedTime "%s%Q%z")  "153014400.000+0530"
-- PresentT 1974-11-07 05:30:00 +0530
--

data ParseTimeP (t :: Type) p
type ParseTimePT (t :: Type) p = ParseTimeP' (Hole t) p Id

instance P (ParseTimePT t p) x => P (ParseTimeP t p) x where
  type PP (ParseTimeP t p) x = PP (ParseTimePT t p) x
  eval _ = eval (Proxy @(ParseTimePT t p))

-- | A convenience method to match against many different datetime formats to find the first match
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

-- | A convenience method to match against many different datetime formats to find the first match
--
-- >>> pz @(ParseTimes LocalTime '["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"] "03/11/19 01:22:33") ()
-- PresentT 2019-03-11 01:22:33
--
-- >>> pz @(ParseTimes LocalTime Fst Snd) (["%Y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%B %d %Y %H:%M:%S", "%Y-%m-%dT%H:%M:%S"], "03/11/19 01:22:33")
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
data ParseTimes (t :: Type) p q
type ParseTimesT (t :: Type) p q = ParseTimes' (Hole t) p q

instance P (ParseTimesT t p q) x => P (ParseTimes t p q) x where
  type PP (ParseTimes t p q) x = PP (ParseTimesT t p q) x
  eval _ = eval (Proxy @(ParseTimesT t p q))

-- | create a 'Day' from three int values passed in as year month and day
--
-- >>> pz @(MkDay' Fst Snd Thd) (2019,99,99999)
-- PresentT Nothing
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

-- | create a 'Day' from three int values passed in as year month and day
--
-- >>> pz @(MkDay '(1,2,3) >> 'Just Id) ()
-- PresentT 0001-02-03
--
-- >>> pz @('Just (MkDay '(1,2,3))) 1
-- PresentT 0001-02-03
--
-- >>> pz @(MkDay Id) (2019,12,30)
-- PresentT (Just 2019-12-30)
--
-- >>> pz @(MkDay Id) (1999,3,13)
-- PresentT (Just 1999-03-13)
--
data MkDay p
type MkDayT p = p >> MkDay' Fst Snd Thd

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


-- | create a 'Day', week number, and the day of the week from three numbers passed in as year month and day
--
-- >>> pz @(MkDayExtra' Fst Snd Thd) (2019,99,99999)
-- PresentT Nothing
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

-- | create a 'Day', week number, and the day of the week from three numbers passed in as year month and day
--
-- >>> pz @(MkDayExtra '(1,2,3) >> 'Just Id >> Fst) ()
-- PresentT 0001-02-03
--
-- >>> pz @(L1 (Just (MkDayExtra '(1,2,3)))) 1
-- PresentT 0001-02-03
--
-- >>> pz @(MkDayExtra Id) (2019,12,30)
-- PresentT (Just (2019-12-30,1,1))
--
-- >>> pz @(MkDayExtra Id) (1999,3,13)
-- PresentT (Just (1999-03-13,10,6))
--
data MkDayExtra p
type MkDayExtraT p = p >> MkDayExtra' Fst Snd Thd

instance P (MkDayExtraT p) x => P (MkDayExtra p) x where
  type PP (MkDayExtra p) x = PP (MkDayExtraT p) x
  eval _ = eval (Proxy @(MkDayExtraT p))

-- | get the day of the week
--
-- >>> pz @('Just (MkDay '(2020,7,11)) >> '(UnMkDay Id, ToWeekYear Id,ToWeekDate Id)) ()
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
            dowString = case dow `mod` 7 of
                          0 -> "Sunday"
                          1 -> "Monday"
                          2 -> "Tuesday"
                          3 -> "Wednesday"
                          4 -> "Thursday"
                          5 -> "Friday"
                          6 -> "Saturday"
                          o -> errorInProgram $ "ToWeekDate:" ++ show o
        in mkNode opts (PresentT (dow,dowString)) (show01 opts msg0 dow p) [hh pp]

-- | get week number of the year
--
-- >>> pz @('Just (MkDay '(2020,7,11)) >> ToWeekYear Id) ()
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
-- >>> pz @(ReadP UTCTime Id >> ToDay) "2020-07-06 12:11:13Z"
-- PresentT 2020-07-06
--
data ToDay
instance ( ToDayC x
         , Show x
         ) => P ToDay x where
  type PP ToDay x = Day
  eval _ opts x =
    let msg0 = "ToDay"
        ret = getDay x
    in pure $ mkNode opts (PresentT ret) (show01 opts msg0 ret x) []

-- | extract 'TimeOfDay' from DateTime
--
-- >>> pz @(ReadP UTCTime Id >> ToTime) "2020-07-06 12:11:13Z"
-- PresentT 12:11:13
--
data ToTime

instance ( ToTimeC x
         , Show x
         ) => P ToTime x where
  type PP ToTime x = TimeOfDay
  eval _ opts x =
    let msg0 = "ToTime"
        ret = getTime x
    in pure $ mkNode opts (PresentT ret) (show01 opts msg0 ret x) []


-- | create a 'TimeOfDay' from three int values passed in as year month and day
--
-- >>> pz @(MkTime' Fst Snd Thd) (13,99,99999)
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
type MkTimeT p = p >> MkTime' Fst Snd Thd

instance P (MkTimeT p) x => P (MkTime p) x where
  type PP (MkTime p) x = PP (MkTimeT p) x
  eval _ = eval (Proxy @(MkTimeT p))


-- | uncreate a 'TimeOfDay' returning hour minute seconds picoseconds
--
-- >>> pz @(ReadP UTCTime "2019-01-01 12:13:14.1234Z" >> ToTime >> UnMkTime Id) ()
-- PresentT (12,13,70617 % 5000)
--
-- >>> pz @(ReadP UTCTime Id >> ToTime >> UnMkTime Id) "2020-07-22 08:01:14.127Z"
-- PresentT (8,1,14127 % 1000)
--
-- >>> pz @(ReadP ZonedTime Id >> '(UnMkDay ToDay, UnMkTime ToTime)) "2020-07-11 11:41:12.333+0400"
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
-- >>> pz @(Rescan "^Date\\((\\d+)([^\\)]+)\\)" >> Head >> Snd >> ReadP Integer (Id !! 0) >> PosixToUTCTime (Id % 1000)) "Date(1530144000000+0530)"
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
-- >>> pz @(Rescan "^Date\\((\\d+)([^\\)]+)\\)" >> Head >> Snd >> ((ReadP Integer (Id !! 0) >> PosixToUTCTime (Id % 1000)) &&& ReadP TimeZone (Id !! 1))) "Date(1530144000000+0530)"
-- PresentT (2018-06-28 00:00:00 UTC,+0530)
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


-- | similar to 'Data.Time.diffUTCTime'
--
-- >>> pz @(DiffUTCTime Fst Snd) (read "2020-11-08 12:12:03Z", read "2020-11-08 11:12:00Z")
-- PresentT 3603s
--
data DiffUTCTime p q

instance (PP p x ~ UTCTime
        , PP q x ~ UTCTime
        , P p x
        , P q x
        ) => P (DiffUTCTime p q) x where
  type PP (DiffUTCTime p q) x = NominalDiffTime
  eval _ opts x = do
    let msg0 = "DiffUTCTime"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = diffUTCTime p q
        in mkNode opts (PresentT b) (msg0 <> " " <> showL opts b <> showVerbose opts " | " p <> showVerbose opts " | " q) [hh pp, hh qq]

-- | similar to 'Data.Time.diffLocalTime'
--
-- >>> pz @(DiffLocalTime Fst Snd) (read "2020-11-08 12:12:03", read "2020-11-05 15:12:00")
-- PresentT 248403s
--
{-
data DiffLocalTime p q

instance (PP p x ~ LocalTime
        , PP q x ~ LocalTime
        , P p x
        , P q x
        ) => P (DiffLocalTime p q) x where
  type PP (DiffLocalTime p q) x = NominalDiffTime
  eval _ opts x = do
    let msg0 = "DiffLocalTime"
    lr <- runPQ msg0 (Proxy @p) (Proxy @q) opts x []
    pure $ case lr of
      Left e -> e
      Right (p,q,pp,qq) ->
        let b = diffUTCTime (localTimeToUTC utc p) (localTimeToUTC utc q)
        in mkNode opts (PresentT b) (msg0 <> " " <> showL opts b <> showVerbose opts " | " p <> showVerbose opts " | " q) [hh pp, hh qq]
-}

data DiffLocalTime p q
type DiffLocalTimeT p q = DiffUTCTime (LocalTimeToUTC p) (LocalTimeToUTC q)

instance P (DiffLocalTimeT p q) x => P (DiffLocalTime p q) x where
  type PP (DiffLocalTime p q) x = PP (DiffLocalTimeT p q) x
  eval _ = eval (Proxy @(DiffLocalTimeT p q))


-- | similar to 'Data.Time.localTimeToUTC'
data LocalTimeToUTC p

instance ( PP p x ~ LocalTime
         , P p x
         ) => P (LocalTimeToUTC p) x where
  type PP (LocalTimeToUTC p) x = UTCTime
  eval _ opts x = do
    let msg0 = "LocalTimeToUTC"
    pp <- eval (Proxy @p) opts x
    pure $ case getValueLR opts msg0 pp [] of
      Left e -> e
      Right p ->
        let d = localTimeToUTC utc p
        in mkNode opts (PresentT d) (show01 opts msg0 d p) [hh pp]

